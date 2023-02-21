package sre.api.finance.cm

import java.time.LocalDateTime
import cats.effect._
import org.http4s.EntityEncoder
import org.http4s.circe._
import io.circe._
import io.circe.syntax._
import io.circe.Encoder

sealed trait CMOtpStatus

object CMOtpStatus {
  case class Validated(transactionId: String, validatedAt: LocalDateTime)
      extends CMOtpStatus
  case class Pending(transactionId: String, requestedAt: LocalDateTime)
      extends CMOtpStatus
  case class Unknown(transactionId: String) extends CMOtpStatus

  implicit val encoder: Encoder[CMOtpStatus] = new Encoder[CMOtpStatus] {
    final def apply(status: CMOtpStatus): Json = {
      status match {
        case s: Validated => "validated".asJson
        case s: Pending   => "pending".asJson
        case s: Unknown   => "unknown".asJson
      }
    }
  }
}
