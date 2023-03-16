package sre.api
package domoticz

import org.http4s.Uri
import cats.effect._
import cats.implicits._
import io.circe._
import io.circe.generic.semiauto._
import JsonImplicits._

case class DomoticzSettings(
  baseUri: Uri,
  wsUri: Uri,
  username: String,
  password: String
)

object DomoticzSettings {

  implicit val decoder: Decoder[DomoticzSettings] = deriveDecoder[DomoticzSettings]

  def fromEnv[F[_]]()(implicit F: Sync[F]): F[DomoticzSettings] = F.pure {
    DomoticzSettings(
      baseUri = Env.getUriOrFail("DOMOTICZ_BASEURI"),
      wsUri = Env.getUriOrFail("DOMOTICZ_WSURI"),
      username = Env.getStringOrFail("DOMOTICZ_USERNAME"),
      password = Env.getStringOrFail("DOMOTICZ_PASSWORD")
    )
  }
}
