package sre.api.finance.cm

import cats.effect._
import java.time.LocalDateTime
import org.http4s.circe._
import org.http4s.EntityEncoder
import io.circe.generic.semiauto._
import io.circe.{ Encoder, Decoder }

case class CMOtpRequest(transactionId: String, requestedAt: LocalDateTime, apkId: String)

object CMOtpRequest {
  implicit val encoder: Encoder[CMOtpRequest] = deriveEncoder[CMOtpRequest]
  implicit val decoder: Decoder[CMOtpRequest] = deriveDecoder[CMOtpRequest]
}
