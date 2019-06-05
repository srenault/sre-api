package sre.api.energy

import java.time.LocalDate
import cats.effect._
import io.circe._
import org.http4s.EntityEncoder
import io.circe.Decoder
import io.circe.generic.semiauto._
import org.http4s.circe._

case class ElectrictyConsumption(date: LocalDate, hp: Float, hc: Float)

object ElectrictyConsumption {

  implicit val decoder: Decoder[ElectrictyConsumption] = new Decoder[ElectrictyConsumption] {

    final def apply(c: HCursor): Decoder.Result[ElectrictyConsumption] =
      for {
        d <- c.downField("d").as[LocalDate]
        hp <- c.downField("v").as[String].map(_.toFloat)
        hc <- c.downField("v2").as[String].map(_.toFloat)
      } yield ElectrictyConsumption(d, hp, hc)
  }

  implicit val encoder: Encoder[ElectrictyConsumption] = deriveEncoder[ElectrictyConsumption]
  implicit def entityEncoder[F[_]: Effect]: EntityEncoder[F, ElectrictyConsumption] = jsonEncoderOf[F, ElectrictyConsumption]
  implicit def entitiesEncoder[F[_]: Effect]: EntityEncoder[F, List[ElectrictyConsumption]] = jsonEncoderOf[F, List[ElectrictyConsumption]]
}
