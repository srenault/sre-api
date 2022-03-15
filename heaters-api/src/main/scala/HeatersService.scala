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

class HeatersService[F[_]: Concurrent](heatersClient: HeatersClient[F], settings: HeatersSettings) extends Http4sDsl[F] {

  object ChannelVar {
    def unapply(str: String): Option[Int] = {
      scala.util.Try(str.toInt).toOption
    }
  }

  private def buildStatusResponse(channels: SortedSet[ChannelStatus]): Json = {
    json"""{ "channels": $channels, "modes": ${Mode.all} }"""
  }

  def routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "heaters" / "status" =>
      heatersClient.getStatus().flatMap { channels =>
        val json = buildStatusResponse(channels)
        Ok(json, `Content-Type`(MediaType.application.json, Charset.`UTF-8`))
      }

    case req @ PUT -> Root / "heaters" / "channel" / ChannelVar(channel) =>
      req.as[Json].flatMap { json =>
        json.hcursor.downField("mode").as[Int].flatMap(Mode.validate) match {
          case Right(mode) =>
            heatersClient.update(channel, mode).flatMap { channels =>
              val json = buildStatusResponse(channels)
              Ok(json, `Content-Type`(MediaType.application.json, Charset.`UTF-8`))
            }

          case Left(_) =>
            BadRequest()
        }
      }

    case req =>
      NotFound(req.toString)
  }
}
