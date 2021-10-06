package sre.api
package energy
package electricity

import java.time.LocalDate
import cats.effect._
import io.circe._
import org.http4s.EntityEncoder
import io.circe.generic.semiauto._
import org.http4s.circe._

case class PowerUsage(date: LocalDate, hp: Float, hc: Float)

object PowerUsage {
  implicit val encoder: Encoder[PowerUsage] = deriveEncoder[PowerUsage]
}

case class PowerConsumption(hpTotalUsage: Float, hcTotalUsage: Float, dailyUsage: List[PowerUsage])

object PowerConsumption {

  def empty = PowerConsumption(hpTotalUsage = 0F, hcTotalUsage = 0F, dailyUsage = Nil)

  implicit val encoder: Encoder[PowerConsumption] = deriveEncoder[PowerConsumption]
  implicit def entityEncoder[F[_]: Effect]: EntityEncoder[F, PowerConsumption] = jsonEncoderOf[F, PowerConsumption]
}
