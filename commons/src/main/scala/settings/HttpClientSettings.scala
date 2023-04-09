package sre.api
package settings

import io.circe._
import io.circe.generic.semiauto._

case class HttpClientSettings(
    logRequest: Boolean,
    logResponse: Boolean
)

object HttpClientSettings {

  implicit val decoder: Decoder[HttpClientSettings] =
    deriveDecoder[HttpClientSettings]
}
