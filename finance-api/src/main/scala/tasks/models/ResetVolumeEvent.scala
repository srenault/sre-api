package sre.api.finance.tasks.models

import io.circe.Decoder
import io.circe.generic.semiauto._
import sre.api.finance.cm.CMOtpRequest

final case class ResetVolumeEvent(continuationToken: Option[String])

object ResetVolumeEvent {
  implicit val decoder: Decoder[ResetVolumeEvent] =
    deriveDecoder[ResetVolumeEvent]
}
