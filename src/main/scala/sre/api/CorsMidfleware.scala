package sre.api

import cats.effect._
import org.http4s._

object CorsMiddleware {

  def addCorsHeader[F[_]: Effect](resp: Response[F]) = {
    val accessControlOrigin = Header("Access-Control-Allow-Origin", "*")
    val accessControlAllowMethods = Header("Access-Control-Allow-Methods", "GET")
    resp match {
      case Status.Successful(resp) => resp.putHeaders(accessControlOrigin, accessControlAllowMethods)
      case resp => resp
    }
  }

  def apply[F[_]: Effect](service: HttpService[F]) =
    service.map(addCorsHeader(_))
}
