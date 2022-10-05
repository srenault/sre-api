package sre.api.finance.tasks.models

import io.circe.Decoder
import io.circe.generic.semiauto._

final case class ReindexEvent(fromScratch: Boolean)

object ReindexEvent {
    implicit val decoder: Decoder[ReindexEvent] = deriveDecoder[ReindexEvent]
}