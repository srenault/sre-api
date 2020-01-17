package sre.api

import cats.effect._
import org.http4s._

object CorsMiddleware {

  def addCorsHeader[F[_]: Effect](resp: Response[F]) = {
    val accessControlOrigin = Header("Access-Control-Allow-Origin", "*")
    val accessControlAllowMethods = Header("Access-Control-Allow-Methods", "*")
    val accessControlAllowHeaders = Header("Access-Control-Allow-Headers", "Authorization")
    val accessControlAllowCredentials = Header("Access-Control-Allow-Credentials", "true")

    resp.putHeaders(
      accessControlOrigin,
      accessControlAllowMethods,
      accessControlAllowCredentials,
      accessControlAllowHeaders
    )
  }

  def apply[F[_]: Effect](service: HttpService[F]) =
    service.map(addCorsHeader(_))
}
