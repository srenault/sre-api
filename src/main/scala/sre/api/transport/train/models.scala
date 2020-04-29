package sre.api.transport.train

import cats.effect._
import org.http4s.{ EntityDecoder, EntityEncoder }
import io.circe._
import io.circe.literal._
import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto._
import org.http4s.circe._

case class AuthResponse(token: String, secret: String)
object AuthResponse {
  implicit val encoder: Encoder[AuthResponse] = deriveEncoder[AuthResponse]
  implicit val decoder: Decoder[AuthResponse] = deriveDecoder[AuthResponse]
  implicit def entityEncoder[F[_]: Effect]: EntityEncoder[F, AuthResponse] = jsonEncoderOf[F, AuthResponse]
  implicit def entityDecoder[F[_]: Effect]: EntityDecoder[F, AuthResponse] = jsonOf[F, AuthResponse]
}

case class Station(
  id: String,
  uic: String,
  label: String,
  longitude: Double,
  latitude: Double,
  `type`: String
)
object Station {

  implicit val encoder: Encoder[Station] = new Encoder[Station] {
    final def apply(station: Station): Json = {
      val encoder = deriveEncoder[Station]
      encoder(station).deepMerge(json"""
          {
            "country": null,
            "departmentCode": null,
            "shortLabel": null
          }"""
      )
    }
  }

  implicit val decoder: Decoder[Station] = deriveDecoder[Station]
  implicit def entitiesEncoder[F[_]: Effect]: EntityEncoder[F, List[Station]] = jsonEncoderOf[F, List[Station]]
}

case class Itinerary(itinerarySteps: List[ItineraryStep])
object Itinerary {
  import io.circe.generic.auto._

  implicit val encoder: Encoder[Itinerary] = deriveEncoder[Itinerary]
  implicit val decoder: Decoder[Itinerary] = deriveDecoder[Itinerary]
  implicit def entitiesEncoder[F[_]: Effect]: EntityEncoder[F, List[Itinerary]] = jsonEncoderOf[F, List[Itinerary]]
}

case class ItineraryStep(arrivalDate: String, departureDate: String, duration: Int, stops: List[Stop], cancelled: Boolean, disruptionsList: List[StopDisruption])
case class Stop(
  departureDate: Option[String],
  arrivalDate: Option[String],
  longitude: Double,
  latitude: Double,
  stationLabel: String,
  stationUic: String
)

case class NextDepartures(disruptions: List[LineDisruption], board: List[NextDeparture])
object NextDepartures {
  import io.circe.generic.auto._
  implicit val encoder: Encoder[NextDepartures] = deriveEncoder[NextDepartures]
  implicit val decoder: Decoder[NextDepartures] = deriveDecoder[NextDepartures]
  implicit def entityEncoder[F[_]: Effect]: EntityEncoder[F, NextDepartures] = jsonEncoderOf[F, NextDepartures]

}

case class NextDeparture(
  destinationStationName: String,
  actualTrainDate: Option[String],
  trainDate: String,
  disruptions: Option[List[StopDisruption]]
)

object NextDeparture {
  import io.circe.generic.auto._
  implicit val encoder: Encoder[NextDeparture] = deriveEncoder[NextDeparture]
  implicit val decoder: Decoder[NextDeparture] = deriveDecoder[NextDeparture]
  implicit def entitiesEncoder[F[_]: Effect]: EntityEncoder[F, List[NextDeparture]] = jsonEncoderOf[F, List[NextDeparture]]
}

case class StopDisruption(scope: String, `type`: String, description: String)

case class LineDisruption(`type`: String, description: String)

case class NearestStop(
  types: List[String],
  longitude: Double,
  latitude: Double,
  ratpStopId: Option[String],
  label: String,
  metadata: Option[NearestStopMeta]
)

object NearestStop {

  import io.circe.generic.auto._

  implicit val encoder: Encoder[NearestStop] = deriveEncoder[NearestStop]
  implicit val decoder: Decoder[NearestStop] = deriveDecoder[NearestStop]
  implicit def entitiesEncoder[F[_]: Effect]: EntityEncoder[F, List[NearestStop]] = jsonEncoderOf[F, List[NearestStop]]
  implicit def entityDecoder[F[_]: Effect]: EntityDecoder[F, NearestStop] = jsonOf[F, NearestStop]
}

case class NearestStopMeta(transporters: List[NearestStopTransporter])

case class NearestStopTransporter(
  gtfsRouteId: Option[String],
  isTerminus: Option[Boolean],
  direction: Option[String],
  line: Option[String],
  `type`: String
)
