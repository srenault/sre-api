package sre.api.finance
package cm

import cats.data.EitherT
import cats.effect._
import cats.implicits._
import org.http4s._
import org.http4s.headers._
import org.http4s.dsl.io._
import org.http4s.client._
import org.http4s.client.dsl.Http4sClientDsl
import org.typelevel.log4cats.Logger
import sre.api.settings.CMSettings

trait CMClientDsl[F[_]] extends Http4sClientDsl[F] with CMOtpClientDsl[F] {

  val logger: Logger[F]

  def httpClient: Client[F]

  def settings: CMSettings

  def basicAuthSessionRef: Ref[F, Option[Deferred[F, CMBasicAuthSession]]]

  def otpSessionRef: Ref[F, Option[Deferred[F, CMOtpSession]]]

  protected def doBasicAuth()(implicit
      F: Concurrent[F]
  ): F[CMBasicAuthSession] = {
    val body = UrlForm(
      "_cm_user" -> settings.username,
      "_cm_pwd" -> settings.password,
      "_charset_" -> "windows-1252",
      "flag" -> "password"
    )

    for {
      maybeValidOtpSession <- getOtpSession().map {
        case Some(otpSession: CMValidOtpSession) => Some(otpSession)
        case _                                   => None
      }

      maybeAuthClientStateCookie = maybeValidOtpSession.map(otpSession =>
        headers.Cookie(otpSession.authClientStateCookie)
      )
      _ = println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
      _ = println(maybeAuthClientStateCookie)
      basicAuthSession <- {
        val authenticationRequest = POST(
          body,
          settings.authenticationUri,
          maybeAuthClientStateCookie.toList
        )
        httpClient.run(authenticationRequest).use { response =>
          val (maybeIdSesCookie, otherCookies) = response.cookies
            .partition(_.name == CMBasicAuthSession.IDSES_COOKIE)
          val idSesCookie = maybeIdSesCookie.headOption
            .getOrElse(sys.error("Unable to get cm basic auth session"))
          val cmBasicAuthSession =
            CMBasicAuthSession.create(idSesCookie, otherCookies)
          F.pure(cmBasicAuthSession)
        }
      }

    } yield basicAuthSession
  }

  private def refreshBasicAuthSession()(implicit
      F: Concurrent[F]
  ): F[CMBasicAuthSession] = {
    logger.info("Requesting cm basic auth session...")
    for {
      d <- Deferred[F, CMBasicAuthSession]
      _ <- basicAuthSessionRef.set(Some(d))
      basicAuthSession <- doBasicAuth()
      _ <- d.complete(basicAuthSession)
    } yield {
      logger.info("Got a new cm basic auth session")
      basicAuthSession
    }
  }

  protected def getOrCreateBasicAuthSession()(implicit
      F: Concurrent[F]
  ): F[CMBasicAuthSession] = {
    for {
      maybeBasicAuthSession <- basicAuthSessionRef.get

      basicAuthSession <- maybeBasicAuthSession match {
        case Some(deferredBasicAuthSession) => deferredBasicAuthSession.get
        case None                           => refreshBasicAuthSession()
      }
    } yield basicAuthSession
  }

  private def refreshSession()(implicit
      F: Concurrent[F]
  ): EitherT[F, CMOtpRequest, CMSession] = {
    for {
      basicAuthSession <- EitherT.liftF(refreshBasicAuthSession())

      currentOtpSession <- EitherT(getOrRequestOtpSession().map {
        case otpSession: CMValidOtpSession => Right(otpSession)
        case otpSession: CMPendingOtpSession =>
          Left(otpSession.toOtpRequest(settings.apkId))
      })

      otpExpired <- EitherT.liftF(
        isOtpSessionExpired(basicAuthSession, currentOtpSession)
      )

      otpSession <- (
        if (otpExpired) {
          EitherT.liftF[F, CMOtpRequest, CMPendingOtpSession](
            requestOtpSession()
          )
        } else
          EitherT.liftF[F, CMOtpRequest, CMOtpSession](
            F.pure(currentOtpSession)
          )
      ).widen[CMOtpSession]

    } yield CMSession(basicAuthSession, otpSession)
  }

  private def getSession()(implicit F: Concurrent[F]): F[CMSession] = {
    for {
      basicAuthSession <- getOrCreateBasicAuthSession()
      otpSession <- getOrRequestOtpSession()
    } yield CMSession(basicAuthSession, otpSession)
  }

  def authenticatedFetch[A](request: Request[F], retries: Int = 1)(
      f: Response[F] => F[A]
  )(implicit F: Concurrent[F]): EitherT[F, CMOtpRequest, A] = {
    logger.info(s"Performing request ${request.uri} with retries = $retries")

    EitherT.liftF(getSession()).flatMap {
      case CMSession(session, otpSession: CMPendingOtpSession) =>
        EitherT.left(F.pure(otpSession.toOtpRequest(settings.apkId)))

      case CMSession(session, otpSession: CMValidOtpSession) =>
        val authenticatedRequest = {
          val cookieHeader = headers.Cookie(session.idSesCookie)
          request.putHeaders(cookieHeader)
        }

        EitherT[F, CMOtpRequest, A] {
          httpClient.run(authenticatedRequest).use { response =>
            val hasExpiredBasicAuthSession =
              response.headers.get[Location].exists { location =>
                location.uri == settings.authenticationUri
              }

            val hasExpiredOtpSession =
              response.headers.get[Location].exists { location =>
                location.uri.toString.endsWith(settings.validationPath.toString)
              }

            if (hasExpiredBasicAuthSession && retries > 0) {
              refreshSession().value
                .flatMap(_ => authenticatedFetch(request, retries - 1)(f).value)
            } else if (hasExpiredBasicAuthSession && retries > 0) {
              sys.error("Unable to refresh cm session")
            } else if (hasExpiredOtpSession) {
              requestOtpSession().map(otpSession =>
                Left(otpSession.toOtpRequest(settings.apkId))
              )
            } else if (response.status == Status.Ok) {
              logger.info(s"Request ${request.uri} OK")
              f(response).map(result => Right[CMOtpRequest, A](result))
            } else {
              sys.error(
                s"An error occured while performing $authenticatedRequest\n:$response"
              )
            }
          }
        }
    }
  }

  def authenticatedGet[A](uri: Uri)(
      f: Response[F] => F[A]
  )(implicit F: Concurrent[F]): EitherT[F, CMOtpRequest, A] = {
    val request = GET(uri)
    authenticatedFetch(request)(f)
  }

  def authenticatedPost[A](uri: Uri, data: UrlForm)(
      f: Response[F] => F[A]
  )(implicit F: Concurrent[F]): EitherT[F, CMOtpRequest, A] = {
    val request = POST(data, uri)
    authenticatedFetch(request)(f)
  }
}
