package sre.api

import io.circe._
import org.http4s.circe._
import org.http4s._
import org.http4s.dsl.Http4sDsl
import cats.effect._
import cats.implicits._
import heaters._

class HeatersService[F[_]: Effect](heatersClient: HeatersClient[F], settings: Settings) extends Http4sDsl[F] {

  object ChannelVar {
    def unapply(str: String): Option[Int] = {
      scala.util.Try(str.toInt).toOption
    }
  }

  val service: HttpRoutes[F] = CorsMiddleware(settings) {
    HttpRoutes.of[F] {
      case GET -> Root / "status" =>
        heatersClient.getStatus().flatMap { status =>
          Ok(status)
        }

      case req @ PUT -> Root / "channel" / ChannelVar(channel) =>
        req.as[Json].flatMap { json =>
          json.hcursor.downField("mode").as[Int].flatMap(Mode.validate) match {
            case Right(mode) =>
              heatersClient.update(channel, mode).flatMap { status =>
                Ok(status)
              }

            case Left(_) =>
              BadRequest()
          }
        }
    }
  }
}
