package sre.api.finance.tasks.models

import io.circe.Encoder
import io.circe.generic.semiauto._
import sre.api.finance.cm.CMOtpStatus

final case class ResetVolumeResult()

object ResetVolumeResult {
    implicit val encoder: Encoder[ResetVolumeResult] = deriveEncoder[ResetVolumeResult]
}
