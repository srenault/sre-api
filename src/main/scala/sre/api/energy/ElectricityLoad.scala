package sre.api.energy

import java.time.LocalDateTime
import cats.effect._
import io.circe._
import org.http4s.EntityEncoder
import io.circe.Decoder
import io.circe.generic.semiauto._
import org.http4s.circe._

case class ElectricityLoad(date: LocalDateTime, value: Float)

object ElectricityLoad {

  implicit val localDateTimeDecoder = new Decoder[LocalDateTime] {
    final def apply(c: HCursor): Decoder.Result[LocalDateTime] = {
      val dateTimeFormatter = java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
      c.as[String].map(LocalDateTime.parse(_, dateTimeFormatter))
    }
  }

  implicit val decoder: Decoder[ElectricityLoad] = new Decoder[ElectricityLoad] {

    final def apply(c: HCursor): Decoder.Result[ElectricityLoad] =
      for {
        time <- c.downField("d").as[LocalDateTime](localDateTimeDecoder)
        value <- c.downField("v").as[String].map(_.toFloat)
      } yield ElectricityLoad(time, value)
  }

  implicit val encoder: Encoder[ElectricityLoad] = deriveEncoder[ElectricityLoad]
  implicit def entityEncoder[F[_]: Effect]: EntityEncoder[F, ElectricityLoad] = jsonEncoderOf[F, ElectricityLoad]
  implicit def entitiesEncoder[F[_]: Effect]: EntityEncoder[F, List[ElectricityLoad]] = jsonEncoderOf[F, List[ElectricityLoad]]
}
