package sre.api

import cats.effect._
import org.http4s._
import org.http4s.server.middleware._
import scala.concurrent.duration._

object CorsMiddleware {

  def apply[F[_]: Async](
      settings: Settings
  )(service: HttpRoutes[F]): HttpRoutes[F] = {
    if (settings.cors) {
      CORS.policy.withAllowOriginAll
        .withAllowMethodsIn(Set(Method.GET, Method.POST, Method.PUT))
        .withAllowCredentials(true)
        .withMaxAge(1.day)
        .apply(service)
    } else {
      service
    }
  }
}
