package sre.api.shutters

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
import sre.api.settings.{ShuttersSettings, ShutterSettings}
import sre.api.domoticz.DomoticzClient
import sre.api.shutters.Action

class ShuttersHttpService[F[_]: Async](
    domoticzClient: DomoticzClient[F],
    settings: ShuttersSettings
) extends Http4sDsl[F] {

  val shuttersService = new ShuttersService(domoticzClient, settings)

  object IdVar {
    def unapply(str: String): Option[Int] = {
      scala.util.Try(str.toInt).toOption
    }
  }

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {

    case req @ GET -> Root / "shutters" =>
      val json = ShuttersHttpService.infoEncoder.apply(settings.info)
      Ok(json)

    case req @ PUT -> Root / "shutters" / IdVar(id) =>
      req.as[Json].flatMap { json =>
        json.hcursor
          .downField("action")
          .as[String]
          .flatMap(Action.validate) match {
          case Right(action) =>
            shuttersService.update(id, action) *> Ok()

          case Left(_) =>
            BadRequest()
        }
      }

    case req =>
      NotFound(req.toString)
  }
}

object ShuttersHttpService {

  val infoEncoder: Encoder[Set[ShutterSettings]] =
    new Encoder[Set[ShutterSettings]] {
      final def apply(shuttersSettings: Set[ShutterSettings]): Json = {
        json"""{ "shutters": $shuttersSettings, "action": ${Action.all} }"""
      }
    }
}
