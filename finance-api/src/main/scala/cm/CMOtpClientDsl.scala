package sre.api.finance.cm

import scala.concurrent.duration._
import scala.xml.Elem
import scala.jdk.CollectionConverters._
import cats.effect._
import cats.implicits._
import org.http4s._
import org.http4s.headers._
import org.http4s.dsl.io._
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.scalaxml._
import fs2.Stream

trait CMOtpClientDsl[F[_]] extends Http4sClientDsl[F] {

  self: CMClientDsl[F] =>

  def otpSessionFile: CMOtpSessionFile[F]

  private def startOtpValidation()(implicit F: Concurrent[F]): F[CMPendingOtpSession] = {
    for {
      basicAuthSession <- getOrCreateBasicAuthSession()

      request = GET(settings.validationUri, headers.Cookie(basicAuthSession.cookies))

      pendingOtpSession <- httpClient.run(request).use { response =>

        response.as[String].map { otpPage =>
          val doc = org.jsoup.Jsoup.parse(otpPage)

          val action = doc.select(s"form").asScala.headOption.flatMap(d => Option(d.attributes.get("action"))) getOrElse {
            sys.error("Unable to get validation form action")
          }

          val otp = doc.select(s"""[name="${CMPendingOtpSession.OTP_HIDDEN_FIELD_ID}"]""").asScala.headOption.flatMap(d => Option(d.attributes.get("value"))) getOrElse {
            sys.error("Unable to get otp value")
          }

          val inAppSendNew1 = doc.select(s"""[name="${CMPendingOtpSession.IN_APP_SEND_NEW1_FIELD_ID}"]""").asScala.headOption.flatMap(d => Option(d.attributes.get("value"))) getOrElse {
            sys.error("Unable to get in app send new 1")
          }

          val inAppSendNew2 = doc.select(s"""[name="${CMPendingOtpSession.IN_APP_SEND_NEW2_FIELD_ID}"]""").asScala.headOption.flatMap(d => Option(d.attributes.get("value"))) getOrElse {
            sys.error("Unable to get in app send new 2")
          }

          val backup = doc.select(s"""[name="${CMPendingOtpSession.GLOBAL_BACKUP_FIELD_ID}"]""").asScala.headOption.flatMap(d => Option(d.attributes.get("value"))) getOrElse {
            sys.error("Unable to get backup value")
          }

          val transactionId = {
            val r = "transactionId: '([^']+)".r
            r.findFirstMatchIn(otpPage).flatMap(m => Option(m.group(1))) getOrElse {
              sys.error("Unable to get transaction id")
            }
          }

          val (maybeOtpAuthCookie, otherCookies) = response.cookies.partition(_.name == CMPendingOtpSession.OTPAUTH_COOKIE)

          val otpAuthCookie = maybeOtpAuthCookie.headOption.getOrElse(sys.error("Unable to get otp session"))

          CMPendingOtpSession.create(action, otp, backup, inAppSendNew1, inAppSendNew2, transactionId, otpAuthCookie, otherCookies)
        }
      }
    } yield pendingOtpSession
  }

  private def checkOtpTransaction(transactionId: String)(implicit F: Concurrent[F]): F[Boolean] = {
    logger.info(s"Checking OTP status for transaction $transactionId")
    val data = UrlForm("transactionId" -> transactionId)
    val uri = settings.transactionUri
    for {
      basicAuthSession <- getOrCreateBasicAuthSession()
      request = POST(data, uri, headers.Cookie(basicAuthSession.idSesCookie))
      xml <- httpClient.expect[Elem](request)
    } yield {
      val isValidated = (xml \ "transactionState").exists(_.text === "VALIDATED")
      val status = (xml \ "code_retour").exists(_.text === "0000")
      isValidated && status
    }
  }

  private def validateOtpSession(pendingOtpSession: CMPendingOtpSession)(implicit F: Concurrent[F]): F[CMValidOtpSession] = {
    logger.info(s"Validating OTP session for transaction ${pendingOtpSession.transactionId}")

    for {
      basicAuthSession <- getOrCreateBasicAuthSession()

      cookies = (basicAuthSession.cookies ++ pendingOtpSession.cookies.toList)

      validatedOtpSession <- {
        val cookieHeader = headers.Cookie(cookies)

        val uri = settings.baseUri.withPath(pendingOtpSession.action)

        val data = UrlForm(
          CMPendingOtpSession.OTP_HIDDEN_FIELD_ID -> pendingOtpSession.otpHidden,
          CMPendingOtpSession.GLOBAL_BACKUP_FIELD_ID -> pendingOtpSession.globalBackup,
          CMPendingOtpSession.IN_APP_SEND_NEW1_FIELD_ID -> pendingOtpSession.inAppSendNew1,
          CMPendingOtpSession.IN_APP_SEND_NEW2_FIELD_ID -> pendingOtpSession.inAppSendNew2,
          CMPendingOtpSession.FID_DO_VALIDATE_X_FIELD,
          CMPendingOtpSession.FID_DO_VALIDATE_Y_FIELD,
          CMPendingOtpSession.WXF2_CC_FIELD
        )

        val request = POST(data, uri, cookieHeader)

        httpClient.run(request).use { response =>
          println(response.status)
          response.cookies.foreach(println)
          val cookie = response.cookies.find(_.name == CMValidOtpSession.AUTH_CLIENT_STATE) getOrElse sys.error("Unable to get otp session")
          val validOtpSession = pendingOtpSession.validate(cookie)
          F.pure(validOtpSession)
        }
      }
    } yield {
      logger.info(s"Otp session for transaction ${pendingOtpSession.transactionId} validated")
      validatedOtpSession
    }
  }

  private def validateOtpTransaction(transactionId: String)(implicit F: Concurrent[F]): F[Unit] = {
    logger.info(s"Validating OTP for transactionId $transactionId ...")
    otpSessionRef.access.flatMap {
      case (value, set) =>
        value match {
          case Some(deferredOtpSession) =>
            deferredOtpSession.get.flatMap {
              case otpSession: CMPendingOtpSession if otpSession.transactionId === transactionId =>
                for {
                  validatedOtpSession <- validateOtpSession(otpSession)
                  d <- Deferred[F, CMOtpSession]
                  _ <- d.complete(validatedOtpSession)
                  result <- set(Some(d))
                  _ <- otpSessionFile.set(validatedOtpSession)
                } yield {
                  logger.info(s"Otp session with transactionId $transactionId validated")
                  ()
                }

              case _ => F.pure(())
            }

          case _ => F.pure(())
        }
    }
  }

  private def resetOtpSession()(implicit F: Concurrent[F]): F[Unit] =
    F.pure {
      logger.info(s"Pending OTP session has been reset.")
      otpSessionFile.delete() *> otpSessionRef.set(None)
    }

  protected def isOtpSessionExpired(basicAuthSession: CMBasicAuthSession, otpSession: CMValidOtpSession)(implicit F: Concurrent[F]): F[Boolean] = {
    val cookieHeader = headers.Cookie(otpSession.authClientStateCookie :: basicAuthSession.cookies)
    val request = GET(settings.homeUri, cookieHeader)
    httpClient.run(request).use { response =>
      F.pure {
        response.headers.get[Location].exists { location =>
          location.uri.toString == settings.validationPath
        }
      }
    }
  }

  protected def requestOtpSession()(implicit F: Concurrent[F]): F[CMPendingOtpSession] = {
    logger.info(s"Requesting new cm OTP session ...")
    for {
      _ <- resetOtpSession()
      d <- Deferred[F, CMOtpSession]
      _ <- otpSessionRef.set(Some(d))
      pendingOtpSession <- startOtpValidation()
      _ <- d.complete(pendingOtpSession)
      //_ <- checkPeriodicallyOtpTransaction(pendingOtpSession.transactionId)
    } yield {
      logger.info(s"A new pending OTP session created with transactionId ${pendingOtpSession.transactionId}")
      pendingOtpSession
    }
  }

  protected def getOtpSession()(implicit F: Concurrent[F]): F[Option[CMOtpSession]] = {
    otpSessionRef.get.flatMap {
      case Some(deferredOtpsession) => deferredOtpsession.get.map(Option(_))
      case None => F.pure(None)
    }
  }

  protected def getOrRequestOtpSession()(implicit F: Concurrent[F]): F[CMOtpSession] = {
    getOtpSession().flatMap {
      case Some(otpSession) => F.pure(otpSession)
      case None => requestOtpSession().widen[CMOtpSession]
    }
  }

  def checkOtpStatus(transactionId: String)(implicit F: Concurrent[F]): F[CMOtpStatus] = {
    otpSessionRef.get.flatMap {
      case Some(deferredOtpSession) =>
        deferredOtpSession.get.flatMap {
          case otpSession if otpSession.transactionId === transactionId =>
            F.pure(otpSession.status)

          case _ => F.pure(CMOtpStatus.Unknown(transactionId))
        }

      case _ => F.pure(CMOtpStatus.Unknown(transactionId))
    }
  }
}
