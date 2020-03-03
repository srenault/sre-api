package sre.api
package energy
package electricity

import java.time.LocalDate
import cats.effect._
import io.circe._
import org.http4s.EntityEncoder
import io.circe.Decoder
import io.circe.generic.semiauto._
import org.http4s.circe._

case class Consumption(date: LocalDate, hp: Float, hc: Float, cost: Float)

object Consumption {

  implicit def decoder(implicit settings: Settings): Decoder[Consumption] = new Decoder[Consumption] {

    final def apply(c: HCursor): Decoder.Result[Consumption] =
      for {
        d <- c.downField("d").as[LocalDate]
        hp <- c.downField("v").as[String].map(_.toFloat)
        hc <- c.downField("v2").as[String].map(_.toFloat)
      } yield {
        val cost = Electricity.computeConsumptionCost(settings.energy.electricity, hc, hp)
        Consumption(d, hp, hc, cost)
      }
  }

  implicit val encoder: Encoder[Consumption] = deriveEncoder[Consumption]
  implicit def entityEncoder[F[_]: Effect]: EntityEncoder[F, Consumption] = jsonEncoderOf[F, Consumption]
  implicit def entitiesEncoder[F[_]: Effect]: EntityEncoder[F, List[Consumption]] = jsonEncoderOf[F, List[Consumption]]
}
