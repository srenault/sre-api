package sre.api.settings

import io.circe._
import io.circe.generic.semiauto._

case class S3Settings(
  region: String,
  bucket: String,
  publicKey: String,
  secretKey: String,
  prefix: String
)

object S3Settings {

  implicit val decoder: Decoder[S3Settings] = deriveDecoder[S3Settings]
}
