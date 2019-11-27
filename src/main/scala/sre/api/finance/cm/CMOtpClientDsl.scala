package sre.api.finance.cm

import scala.concurrent.duration._
import scala.xml.Elem
import scala.collection.JavaConverters._
import cats.effect._
import cats.implicits._
import cats.effect.concurrent.Deferred
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.scalaxml._
import fs2.Stream
import fs2.concurrent.SignallingRef

trait CMOtpClientDsl[F[_]] extends Http4sClientDsl[F] {

  self: CMClientDsl[F] =>

  def otpPollingInterrupter: SignallingRef[F, Boolean]

  def otpSessionFile: CMOtpSessionFile[F]

  private def startOtpValidation()(implicit F: ConcurrentEffect[F]): F[CMPendingOtpSession] = {
    for {
      basicAuthSession <- getOrCreateBasicAuthSession()

      request <- GET(settings.validationUri, headers.Cookie(basicAuthSession.cookies))

      pendingOtpSession <- httpClient.fetch(request) { response =>

        response.as[String].map { otpPage =>
          val doc = org.jsoup.Jsoup.parse(otpPage)

          val otp = doc.select(s"""[name="${CMPendingOtpSession.OTP_HIDDEN_FIELD_ID}"]""").asScala.headOption.flatMap(d => Option(d.attributes.get("value"))) getOrElse {
            sys.error("Unable to get otp value")
          }
          val inApp = doc.select(s"""[name="${CMPendingOtpSession.INPUT_HIDDEN_KEY_IN_APPS_SEND_NEW1_ID}"]""").asScala.headOption.flatMap(d => Option(d.attributes.get("value"))) getOrElse {
            sys.error("Unable to get inApp value")
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

          CMPendingOtpSession.create(otp, inApp, backup, transactionId, otpAuthCookie, otherCookies)
        }
      }
    } yield pendingOtpSession
  }

  private def checkOtpTransaction(transactionId: String)(implicit F: ConcurrentEffect[F]): F[Boolean] = {
    logger.info(s"Checking OTP status for transaction $transactionId")
    val data = UrlForm("transactionId" -> transactionId)
    val uri = settings.transactionUri
    for {
      basicAuthSession <- getOrCreateBasicAuthSession()
      request <- POST(data, uri, headers.Cookie(basicAuthSession.idSesCookie))
      xml <- httpClient.expect[Elem](request)
    } yield {
      val isValidated = (xml \ "transactionState").exists(_.text === "VALIDATED")
      val status = (xml \ "code_retour").exists(_.text === "0000")
      isValidated && status
    }
  }

  private def validateOtpSession(pendingOtpSession: CMPendingOtpSession)(implicit F: ConcurrentEffect[F]): F[CMValidOtpSession] = {
    logger.info(s"Validating OTP session for transaction ${pendingOtpSession.transactionId}")

    for {
      basicAuthSession <- getOrCreateBasicAuthSession()

      cookies = (basicAuthSession.cookies ++ pendingOtpSession.cookies.toList)

      validatedOtpSession <- {
        val cookieHeader = headers.Cookie(cookies)

        val uri = settings.validationUri
          .withQueryParam("_tabi", "C")
          .withQueryParam("_pid", "OtpValidationPage")

        val data = UrlForm(
          CMPendingOtpSession.OTP_HIDDEN_FIELD_ID -> pendingOtpSession.otpHidden,
          CMPendingOtpSession.GLOBAL_BACKUP_FIELD_ID -> pendingOtpSession.globalBackup,
          CMPendingOtpSession.INPUT_HIDDEN_KEY_IN_APPS_SEND_NEW1_ID -> pendingOtpSession.inputHiddenInAppsEndNew1,
          CMPendingOtpSession.FID_DO_VALIDATE_X_FIELD,
          CMPendingOtpSession.FID_DO_VALIDATE_Y_FIELD,
          CMPendingOtpSession.WXF2_CC_FIELD
        )

        val request = POST(data, uri, cookieHeader)

        httpClient.fetch(request) { response =>
          val cookie = response.cookies.find(_.name == CMValidOtpSession.AUTH_CLIENT_STATE) getOrElse sys.error("Unable to get otp session")
          val validOtpSession = pendingOtpSession.validate(cookie)
          F.pure(validOtpSession)
        }
      }

      //_ <- validateBasicAuthSession(basicAuthSession, validatedOtpSession)

    } yield {
      logger.info(s"Otp session for transaction ${pendingOtpSession.transactionId} validated")
      validatedOtpSession
    }
  }

  private def validateOtpTransaction(transactionId: String)(implicit F: ConcurrentEffect[F]): F[Unit] = {
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
                  _ <- otpPollingInterrupter.set(true)
                } yield {
                  logger.info(s"Otp session with transactionId $transactionId validated")
                  Unit
                }

              case _ => F.pure(Unit)
            }

          case _ => F.pure(Unit)
        }
    }
  }

  private def checkPeriodicallyOtpTransaction(transactionId: String)(implicit F: Concurrent[F], P: ConcurrentEffect[F], timer: Timer[F]): F[Fiber[F, Unit]] = {
    val f: F[Unit] = checkOtpTransaction(transactionId).flatMap {
      case true =>
        logger.info(s"OTP validated for transaction $transactionId")
        validateOtpTransaction(transactionId)

      case false =>
        logger.info(s"OTP pending for transaction $transactionId")
        F.pure(Unit)
    }

    val polling = Stream.awakeEvery[F](2.second)
      .zipRight(Stream.eval(f).repeat)
      .interruptWhen(otpPollingInterrupter)
      .interruptAfter(2.minutes)
      .onFinalizeCase {
        case ExitCase.Canceled | ExitCase.Completed =>
          getOtpSession().flatMap {
            case Some(otpSession: CMPendingOtpSession) => // TIMEOUT
              resetOtpSession()

            case _ => Sync[F].unit // VALIDATED
          }

        case ExitCase.Error(e) =>
          logger.warn(s"Unexpected error while polling pending OTP status with transaction $transactionId:\n${e.getMessage}")
          resetOtpSession()
      }

    F.start(polling.compile.drain)
  }

  private def resetOtpSession()(implicit F: Effect[F]): F[Unit] =
    F.defer {
      logger.info(s"Pending OTP session has been reset.")
      otpSessionFile.delete() *> otpSessionRef.set(None) *> otpPollingInterrupter.set(false)
    }

  protected def isOtpSessionExpired(basicAuthSession: CMBasicAuthSession, otpSession: CMValidOtpSession)(implicit F: ConcurrentEffect[F]): F[Boolean] = {
    val cookieHeader = headers.Cookie(otpSession.authClientStateCookie :: basicAuthSession.cookies)
    val request = GET(settings.homeUri, cookieHeader)
    httpClient.fetch(request) { response =>
      F.pure {
        response.headers.get(headers.Location).exists { location =>
          location.value == settings.validationPath.toString
        }
      }
    }
  }

  protected def requestOtpSession()(implicit F: ConcurrentEffect[F], timer: Timer[F]): F[CMPendingOtpSession] = {
    logger.info(s"Requesting new cm OTP session ...")
    for {
      _ <- resetOtpSession()
      d <- Deferred[F, CMOtpSession]
      _ <- otpSessionRef.set(Some(d))
      basicAuthSession <- getOrCreateBasicAuthSession()
      pendingOtpSession <- startOtpValidation()
      _ <- d.complete(pendingOtpSession)
      _ <- checkPeriodicallyOtpTransaction(pendingOtpSession.transactionId)
    } yield {
      logger.info(s"A new pending OTP session created with transactionId ${pendingOtpSession.transactionId}")
      pendingOtpSession
    }
  }

  protected def getOtpSession()(implicit F: ConcurrentEffect[F]): F[Option[CMOtpSession]] = {
    otpSessionRef.get.flatMap {
      case Some(deferredOtpsession) => deferredOtpsession.get.map(Option(_))
      case None => F.pure(None)
    }
  }

  protected def getOrRequestOtpSession()(implicit F: ConcurrentEffect[F], timer: Timer[F]): F[CMOtpSession] = {
    getOtpSession().flatMap {
      case Some(otpSession) => F.pure(otpSession)
      case None => requestOtpSession().widen[CMOtpSession]
    }
  }

  def checkOtpStatus(transactionId: String)(implicit F: ConcurrentEffect[F]): F[CMOtpStatus] = {
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
