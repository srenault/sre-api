package sre.api.transport.train

import org.typelevel.ci._
import cats.effect._
import cats.implicits._
import io.circe._
import io.circe.literal._
import io.circe.syntax._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.client._
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.circe._
import sre.api.utils.Security

trait TrainClientDsl[F[_]] extends Http4sClientDsl[F] {

  def httpClient: Client[F]

  def endpoint: Uri

  def authInfoRef: Ref[F, Option[Deferred[F, AuthResponse]]]

  val USER_AGENT_HEADER = Header.Raw(ci"User-Agent", "SncfFusion Android")

  val CONTENT_TYPE_HEADER = Header.Raw(ci"Content-Type", "application/json")

  val DEFAULT_HEADERS = USER_AGENT_HEADER :: CONTENT_TYPE_HEADER :: Nil

  def authenticate()(implicit F: Async[F]): F[AuthResponse] = {
    val body = json"""
      {
        "anonymousUser": true,
        "deviceIdentifier": "10e7b74c-13b9-44a0-9695-929f15f47532",
        "devicePlatform": "xena",
        "deviceType": "ANDROID",
        "login": "10e7b74c-13b9-44a0-9695-929f15f47532",
        "password": "59f2c132-cb07-425f-b105-9ca391929080"
      }"""

    val request = POST(body, endpoint / "authenticate")
    httpClient.expect[AuthResponse](request)
  }

  def refreshAuthInfo()(implicit F: Async[F]): F[AuthResponse] = {
    for {
      d <- Deferred[F, AuthResponse]
      _ <- authInfoRef.set(Some(d))
      authInfo <- authenticate()
      _ <- d.complete(authInfo)
    } yield authInfo
  }

  def withAuthInfo[A](f: AuthResponse => F[A])(implicit F: Async[F]): F[A] = {
    (for {
      maybeAuthInfo <- authInfoRef.get
      authInfo <- maybeAuthInfo match {
        case Some(deferredAuthInfo) =>
          deferredAuthInfo.get
        case None => refreshAuthInfo()
      }
      res <- f(authInfo).recoverWith {
        case UnexpectedStatus(Status.Unauthorized, _, _) =>
          refreshAuthInfo().flatMap(f)
      }
    } yield res)
  }

  def AuthenticatedPOST[A](
      uri: Uri,
      body: A,
      authInfo: AuthResponse,
      headers: Header.Raw*
  )(implicit
      jsonEncoder: Encoder[A],
      jsonEntityEncoder: EntityEncoder[F, A]
  ): Request[F] = {
    val bodyAsString = body.asJson.noSpaces
    val authHeader = buildAuthHeader(uri, POST, Some(bodyAsString), authInfo)
    POST(body, uri, authHeader +: DEFAULT_HEADERS ++: headers)
  }

  def AuthenticatedGET(
      uri: Uri,
      authInfo: AuthResponse,
      headers: Header.Raw*
  ): Request[F] = {
    val authHeader = buildAuthHeader(uri, GET, body = None, authInfo)
    GET(uri, authHeader +: headers)
  }

  private def buildAuthHeader[A](
      uri: Uri,
      method: Method,
      body: Option[String],
      authInfo: AuthResponse
  ): Header.Raw = {
    val token = authInfo.token
    val secret = authInfo.secret
    val data = body getOrElse ""
    val signedRequest =
      Security.sha1(s"${uri.renderString}:${method.name}:$data")
    val timestamp = System.currentTimeMillis / 1000
    val nonce = java.util.UUID.randomUUID().toString()
    val signature = Security.hmacSha256(
      s"appun:$token:$secret:$timestamp:$nonce:$signedRequest",
      secret
    )
    val value = Map(
      "Mac id" -> token,
      "ts" -> timestamp,
      "nonce" -> nonce,
      "mac" -> signature
    ).map { case (name, value) => s"""$name="$value"""" }.mkString(", ")
    Header.Raw(ci"Authorization", value)
  }
}
