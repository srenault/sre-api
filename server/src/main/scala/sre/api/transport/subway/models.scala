package sre.api.transport.subway

import org.http4s.EntityEncoder
import io.circe.literal._
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto._
import org.http4s.circe._

object Stops {
  val PARIS_MONTPARNASSE = "1824-1825-1826-1827-2363-2364-2365-2366"
  val PARMENTIER = "1779-2389"
}

case class NextDeparture(
    arrivalTimes: List[String],
    direction: String,
    line: String
)

object NextDeparture {
  implicit val encoder: Encoder[NextDeparture] = deriveEncoder[NextDeparture]
  implicit val decoder: Decoder[NextDeparture] = deriveDecoder[NextDeparture]
  implicit def entitiesEncoder[F[_]]: EntityEncoder[F, List[NextDeparture]] =
    jsonEncoderOf[List[NextDeparture]]
}
