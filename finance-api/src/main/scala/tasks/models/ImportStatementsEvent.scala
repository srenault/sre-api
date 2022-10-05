package sre.api.finance.tasks.models

import io.circe.Decoder
import io.circe.generic.semiauto._

final case class ImportStatementsEvent()

object ImportStatementsEvent {
  implicit val decoder: Decoder[ImportStatementsEvent] = deriveDecoder[ImportStatementsEvent]
}
