package sre.api.finance.tasks.models

import io.circe.Decoder
import io.circe.generic.semiauto._

final case class SnapshotEvent(fromScratch: Boolean)

object SnapshotEvent {
    implicit val decoder: Decoder[SnapshotEvent] = deriveDecoder[SnapshotEvent]
}