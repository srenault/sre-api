package sre.api.finance
package tasks.models

import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import analytics.PeriodIndex
import sre.api.finance.cm.CMOtpRequest

final case class ImportStatementsResult(otpRequest: Option[CMOtpRequest])

object ImportStatementsResult {
  implicit val encoder: Encoder[ImportStatementsResult] = deriveEncoder[ImportStatementsResult]
}
