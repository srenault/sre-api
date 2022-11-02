package sre.api.finance.tasks.models

import io.circe.Decoder
import io.circe.generic.semiauto._
import sre.api.finance.cm.CMOtpRequest

final case class SetupVolumeEvent(continuationToken: Option[String])

object SetupVolumeEvent {
  implicit val decoder: Decoder[SetupVolumeEvent] =
    deriveDecoder[SetupVolumeEvent]
}
