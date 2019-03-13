package sre.dashboard.finance

import cats.effect._
import cats.implicits._
import cats.effect.concurrent.{ Ref, Deferred }
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.client._
import org.http4s.client.dsl.Http4sClientDsl
import sre.dashboard.CMSettings
import org.slf4j.Logger

trait CMClientDsl[F[_]] extends Http4sClientDsl[F] {

  val logger: Logger

  def httpClient: Client[F]

  def settings: CMSettings

  def sessionRef: Ref[F, Option[Deferred[F, CMSession]]]

  def login()(implicit F: ConcurrentEffect[F]): F[CMSession] = {
    logger.info("Authenticating...")

    val body = UrlForm(
      "_cm_user" -> settings.username,
      "_cm_pwd" -> settings.password,
      "flag" -> "password"
    )

    val request = POST(body, settings.authenticationUri)

    httpClient.fetch(request) { response =>
      val cookie = response.cookies.find(_.name == "IdSes") getOrElse sys.error("Unable to get cm session")
      val location = response.headers.get(headers.Location) getOrElse sys.error("Unable to get login")
      val redirectUri = GET(location.uri, CMSession(cookie).toRequestCookie)

      httpClient.fetch(redirectUri) { response =>
        logger.info(s"Authentication OK")
        F.pure(CMSession(cookie))
      }
    }
  }

  def refreshSession()(implicit F: ConcurrentEffect[F]): F[CMSession] = {
    logger.info("Refreshing session...")
    for {
      d <- Deferred[F, CMSession]
      _ <- sessionRef.set(Some(d))
      session <- login()
      _ <- d.complete(session)
    } yield session
  }

  def authenticatedFetch[A](request: Request[F], retries: Int = 1)(f: Response[F] => F[A])(implicit F: ConcurrentEffect[F]): F[A] = {
    logger.info(s"Performing request ${request.uri} with retries = $retries")
    withSession { session =>
      httpClient.fetch(request.putHeaders(session.toRequestCookie)) { response =>
        val isUnauthorized = response.headers.get(headers.Location).exists { location =>
          location.value == settings.authenticationUri.toString
        }

        if (isUnauthorized && retries > 0) {
          refreshSession().flatMap { session =>
            authenticatedFetch(request, retries - 1)(f)
          }
        } else if (isUnauthorized) {
          sys.error("Unable to refresh cm session")
        } else if (response.status == Status.Ok){
          logger.info(s"Request ${request.uri} OK")
          f(response)
        } else {
          sys.error(s"An error occured while performing $request\n:$response")
        }
      }
    }
  }

  def withSession[A](f: CMSession => F[A])(implicit F: ConcurrentEffect[F]): F[A] = {
    for {
      maybeSession <- sessionRef.get
      session <- maybeSession match {
        case Some(deferredSession) => deferredSession.get
        case None => refreshSession()
      }
      res <- f(session)
    } yield res
  }

  def doAuthenticatedGET[A](uri: Uri)(f: Response[F] => F[A])(implicit F: ConcurrentEffect[F]): F[A] = {
    GET(uri).flatMap { request =>
      authenticatedFetch(request)(f)
    }
  }

  def doAuthenticatedPOST[A](uri: Uri, data: UrlForm)(f: Response[F] => F[A])(implicit F: ConcurrentEffect[F]): F[A] = {
    POST(data, uri).flatMap { request =>
      authenticatedFetch(request)(f)
    }
  }
}
