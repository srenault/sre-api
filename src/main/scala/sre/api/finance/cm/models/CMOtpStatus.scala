package sre.api.finance.cm

import java.time.LocalDateTime
import cats.effect._
import org.http4s.EntityEncoder
import org.http4s.circe._
import io.circe._
import io.circe.literal._
import io.circe.Encoder

trait CMOtpStatus

object CMOtpStatus {
  case class Validated(transactionId: String, validatedAt: LocalDateTime) extends CMOtpStatus
  case class Pending(transactionId: String, requestedAt: LocalDateTime) extends CMOtpStatus
  case class Unknown(transactionId: String) extends CMOtpStatus

  implicit val encoder: Encoder[CMOtpStatus] = new Encoder[CMOtpStatus] {
    final def apply(status: CMOtpStatus): Json = {
      status match {
        case v: Validated =>
          json"""{ "status": "validated", "validatedAt": ${v.validatedAt}, "transactionId": ${v.transactionId} }"""

        case p: Pending =>
          json"""{ "status": "pending", "requestedAt": ${p.requestedAt}, "transactionId": ${p.transactionId} }"""

        case u: Unknown =>
          json"""{ "status": "unknown" , "transactionId": ${u.transactionId} }"""
      }
    }
  }

  implicit def entityEncoder[F[_]: Effect]: EntityEncoder[F, CMOtpStatus] = jsonEncoderOf[F, CMOtpStatus]
}
