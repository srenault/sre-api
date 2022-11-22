package sre.api
package settings

import org.http4s.Uri
import cats.effect._
import cats.implicits._
import io.circe._
import io.circe.generic.semiauto._
import JsonImplicits._

object HeatersSettings {

  def fromEnv[F[_]]()(implicit F: Sync[F]): F[HeatersSettings] = F.pure {
    HeatersSettings(
      httpClient = HttpClientSettings(
        logRequest = Env.getBooleanOrFail("HTTPCLIENT_LOGREQUEST"),
        logResponse = Env.getBooleanOrFail("HTTPCLIENT_LOGRESPONSE")
      ),
      baseUri = Env.getUriOrFail("HEATERS_BASEURI"),
      username = Env.getStringOrFail("HEATERS_USERNAME"),
      password = Env.getStringOrFail("HEATERS_PASSWORD")
    )
  }

 implicit val decoder: Decoder[HeatersSettings] = deriveDecoder[HeatersSettings]
}

case class HeatersSettings(
  httpClient: HttpClientSettings,
  baseUri: Uri,
  username: String,
  password: String
)
