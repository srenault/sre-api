package sre.api.finance.tasks.models

import io.circe.Encoder
import io.circe.generic.semiauto._
import sre.api.finance.cm.CMOtpStatus

final case class CheckOtpResult(status: CMOtpStatus)

object CheckOtpResult {
    implicit val encoder: Encoder[CheckOtpResult] = deriveEncoder[CheckOtpResult]
}