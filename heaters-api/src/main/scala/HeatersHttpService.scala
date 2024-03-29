package sre.api.heaters

import scala.collection.SortedSet
import io.circe._
import io.circe.literal._
import org.http4s.circe._
import org.http4s._
import org.http4s.dsl.Http4sDsl
import cats.effect._
import cats.implicits._
import org.http4s.headers.`Content-Type`
import org.http4s.syntax.header._
import java.nio.charset.StandardCharsets.UTF_8
import sre.api.settings.HeatersSettings

class HeatersHttpService[F[_]: Async](
    heatersClient: HeatersClient[F],
    settings: HeatersSettings
) extends Http4sDsl[F] {

  val heatersService = new HeatersService(heatersClient, settings)

  object ChannelVar {
    def unapply(str: String): Option[Int] = {
      scala.util.Try(str.toInt).toOption
    }
  }

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "heaters" / "channels" =>
      heatersService.getStatus().flatMap { channels =>
        val json = HeatersHttpService.statusEncoder(channels)
        Ok(json, `Content-Type`(MediaType.application.json, Charset.`UTF-8`))
      }

    case req @ PUT -> Root / "heaters" / "channels" / ChannelVar(channel) =>
      req.as[Json].flatMap { json =>
        json.hcursor.downField("mode").as[Int].flatMap(Mode.validate) match {
          case Right(mode) =>
            heatersService.update(channel, mode).flatMap { channels =>
              val json = HeatersHttpService.statusEncoder(channels)
              Ok(
                json,
                `Content-Type`(MediaType.application.json, Charset.`UTF-8`)
              )
            }

          case Left(_) =>
            BadRequest()
        }
      }

    case req =>
      NotFound(req.toString)
  }
}

object HeatersHttpService {

  val statusEncoder: Encoder[SortedSet[ChannelStatus]] =
    new Encoder[SortedSet[ChannelStatus]] {
      final def apply(channels: SortedSet[ChannelStatus]): Json = {
        json"""{ "channels": $channels, "modes": ${Mode.all} }"""
      }
    }
}
