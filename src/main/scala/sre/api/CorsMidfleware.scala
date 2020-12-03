package sre.api

import cats.effect._
import org.http4s._
import org.http4s.server.middleware._
import scala.concurrent.duration._

object CorsMiddleware {

  def apply[F[_]: Effect](settings: Settings)(service: HttpRoutes[F]): HttpRoutes[F] = {
    if (settings.cors) {
      val config = CORSConfig(
        anyOrigin = true,
        anyMethod = false,
        allowedMethods = Some(Set("GET", "POST", "PUT")),
        allowCredentials = true,
        maxAge = 1.day.toSeconds
      )
     CORS(service, config)
    } else {
      service
    }
  }
}
