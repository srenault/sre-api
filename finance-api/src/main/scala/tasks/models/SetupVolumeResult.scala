package sre.api.finance.tasks.models

import io.circe.Encoder
import io.circe.generic.semiauto._
import sre.api.finance.cm.CMOtpStatus

final case class SetupVolumeResult(continuationToken: Option[String])

object SetupVolumeResult {
  implicit val encoder: Encoder[SetupVolumeResult] =
    deriveEncoder[SetupVolumeResult]
}
