package sre.api
package energy
package electricity

import java.time.LocalDate
import cats.effect._
import io.circe._
import org.http4s.EntityEncoder
import io.circe.generic.semiauto._
import org.http4s.circe._

case class PowerConsumption(date: LocalDate, hp: Float, hc: Float, hpCost: Float, hcCost: Float)

object PowerConsumption {
  implicit val encoder: Encoder[PowerConsumption] = deriveEncoder[PowerConsumption]
  implicit def entityEncoder[F[_]: Effect]: EntityEncoder[F, PowerConsumption] = jsonEncoderOf[F, PowerConsumption]
  implicit def entitiesEncoder[F[_]: Effect]: EntityEncoder[F, List[PowerConsumption]] = jsonEncoderOf[F, List[PowerConsumption]]
}
