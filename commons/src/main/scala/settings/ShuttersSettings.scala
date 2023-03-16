package sre.api
package settings

import org.http4s.Uri
import cats.effect._
import cats.implicits._
import io.circe._
import io.circe.generic.semiauto._
import JsonImplicits._

case class ShutterSettings(id: Int, label: String)

object ShutterSettings {
  implicit val decoder: Decoder[ShutterSettings] = deriveDecoder[ShutterSettings]
  implicit val encoder: Encoder[ShutterSettings] = deriveEncoder[ShutterSettings]
}

object ShuttersSettings {
  implicit val decoder: Decoder[ShuttersSettings] = deriveDecoder[ShuttersSettings]

  def fromEnv[F[_]]()(implicit F: Sync[F]): F[ShuttersSettings] = F.pure {
    ShuttersSettings(
      info = Env.getJsonAsOrFail[Set[ShutterSettings]]("SHUTTERS_CONFIG"),
    )
  }
}

case class ShuttersSettings(
  info: Set[ShutterSettings]
)
