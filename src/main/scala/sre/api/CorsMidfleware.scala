package sre.api

import cats.effect._
import org.http4s._

object CorsMiddleware {

  def addCorsHeader[F[_]: Effect](resp: Response[F]) = {
    val accessControlOrigin = Header("Access-Control-Allow-Origin", "*")
    val accessControlAllowMethods = Header("Access-Control-Allow-Methods", "GET, POST")

    resp.putHeaders(
      accessControlOrigin,
      accessControlAllowMethods,
    )
  }

  def apply[F[_]: Effect](settings: Settings)(service: HttpService[F]) = {
    if (settings.cors) {
      service.map(addCorsHeader(_))
    } else {
      service
    }
  }
}
