package sre.api.weather

import io.circe._
import io.circe.generic.semiauto._
import cats.effect._
import org.http4s.circe._
import org.http4s.EntityEncoder
import java.time._

trait WeatherDateTimeDecoder {
  implicit val zonedDateTimeDecoder = new Decoder[ZonedDateTime] {
    final def apply(c: HCursor): Decoder.Result[ZonedDateTime] = {
      val dateTimeFormatter = java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
      c.as[String].map(LocalDateTime.parse(_, dateTimeFormatter).atZone(ZoneId.of("UTC")))
    }
  }

  implicit val localDateTimeDecoder = new Decoder[LocalDateTime] {
    final def apply(c: HCursor): Decoder.Result[LocalDateTime] = {
      val dateTimeFormatter = java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
      c.as[String].map(LocalDateTime.parse(_, dateTimeFormatter))
    }
  }
}

case class MatchedStation(
  id: String,
  countrycode: String,
  dept: String,
  name: String,
  latitude: Double,
  longitude: Double
)

object MatchedStation {
  implicit val decoder: Decoder[MatchedStation] = deriveDecoder[MatchedStation]
  implicit val encoder: Encoder[MatchedStation] = deriveEncoder[MatchedStation]
  implicit def entitiesEncoder[F[_]: Effect]: EntityEncoder[F, List[MatchedStation]] = jsonEncoderOf[F, List[MatchedStation]]
}

case class Station(
  metadata: Station.Metadata,
  data: List[Station.Metrics]
)

object Station {
  implicit val decoder: Decoder[Station] = deriveDecoder[Station]
  implicit lazy val encoder: Encoder[Station] = deriveEncoder[Station]
  implicit def entityEncoder[F[_]: Effect]: EntityEncoder[F, Station] = jsonEncoderOf[F, Station]

  case class Metadata(
    id: String,
    libelle: String,
    departement: String,
    pays: String,
    pays_nom: String,
    latitude: Double,
    longitude: Double,
    altitude: Double,
    timezone: String,
    temps: String
  )

  object Metadata {
    implicit val decoder: Decoder[Metadata] = deriveDecoder[Metadata]
    implicit val encoder: Encoder[Metadata] = deriveEncoder[Metadata]
  }

  case class Metrics(
    dh_utc: ZonedDateTime,
    temperature: Option[Float],
    ressenti: Option[Float],
    humidite: Option[Int],
    point_de_rosee: Option[Float],
    pression: Option[Float],
    pluie_1h: Float,
    visibilite: Option[Float],
    vent_moyen: Option[Float],
    vent_direction: Option[Float],
    vent_raphales: Option[Float]
  )

  object Metrics extends WeatherDateTimeDecoder {
    implicit val decoder: Decoder[Metrics] = deriveDecoder[Metrics]
    implicit val encoder: Encoder[Metrics] = deriveEncoder[Metrics]

  }
}

case class City(
  countrycode: String,
  dept: String,
  geoid: Long,
  name: String,
  latitude: Double,
  longitude: Double,
  altitude: Int,
  distance: Long,
  bearing: Int,
  weight: String
)

object City {
  implicit val decoder: Decoder[City] = deriveDecoder[City]
  implicit val encoder: Encoder[City] = deriveEncoder[City]
  implicit def entitiesEncoder[F[_]: Effect]: EntityEncoder[F, List[City]] = jsonEncoderOf[F, List[City]]
}

case class Forecast(
  region: Option[String],
  mise_a_jour: LocalDateTime,
  prevision: List[Forecast.Item]
)

object Forecast extends WeatherDateTimeDecoder {
  implicit val decoder: Decoder[Forecast] = deriveDecoder[Forecast]
  implicit val encoder: Encoder[Forecast] = deriveEncoder[Forecast]
  implicit def entityEncoder[F[_]: Effect]: EntityEncoder[F, Forecast] = jsonEncoderOf[F, Forecast]

  case class Temperature(
    mini: Double,
    maxi: Double
  )

  object Temperature {
    implicit val decoder: Decoder[Temperature] = deriveDecoder[Temperature]
    implicit val encoder: Encoder[Temperature] = deriveEncoder[Temperature]
  }

  case class Summary(code: Int, texte: String)

  object Summary {
    implicit val decoder: Decoder[Summary] = deriveDecoder[Summary]
    implicit val encoder: Encoder[Summary] = deriveEncoder[Summary]
  }

  case class Nebulosite(
    totale: Double,
    basse: Double,
    haute: Double,
    moyenne: Double
  )

  object Nebulosite {
    implicit val decoder: Decoder[Nebulosite] = deriveDecoder[Nebulosite]
    implicit val encoder: Encoder[Nebulosite] = deriveEncoder[Nebulosite]
  }

  case class HourlyDetails(
    heure: Int,
    temperature: Double,
    humidite: Int,
    pluie_3h: Double,
    pluie_conv_3h: Double,
    vent_moy: Int,
    rafales: Int,
    pression: Double,
    neige: Boolean,
    point_rosee: Int,
    vent_dir: Option[Int],
    stormmotion: Option[Int],
    nebulosite: Option[Nebulosite]
  )

  object HourlyDetails {
    implicit val decoder: Decoder[HourlyDetails] = deriveDecoder[HourlyDetails]
    implicit val encoder: Encoder[HourlyDetails] = deriveEncoder[HourlyDetails]
  }

  case class Item(
    day: LocalDate,
    saint: String,
    lever_soleil: String,
    coucher_soleil: String,
    temperature: Temperature,
    cumul_pluie: Double,
    vent_max: Int,
    matin: Summary,
    apres_midi: Summary,
    temps: String,
    details: List[HourlyDetails]
  )

  object Item {
    implicit val decoder: Decoder[Item] = deriveDecoder[Item]
    implicit val encoder: Encoder[Item] = deriveEncoder[Item]
  }
}
