package sre.api
package energy
package electricity

import java.time.LocalDate
import io.circe._
import org.http4s.EntityEncoder
import io.circe.generic.semiauto._
import org.http4s.circe._

case class PowerUsage(date: LocalDate, hp: Float, hc: Float)

object PowerUsage {
  implicit val encoder: Encoder[PowerUsage] = deriveEncoder[PowerUsage]
}

case class PowerConsumption(
    startHcCounter: Float,
    endHcCounter: Float,
    startHpCounter: Float,
    endHpCounter: Float,
    dailyUsage: List[PowerUsage]
) {
  val hcTotalUsage = endHcCounter - startHcCounter
  val hpTotalUsage = endHpCounter - startHpCounter
}

object PowerConsumption {

  def empty = PowerConsumption(
    startHcCounter = 0f,
    endHcCounter = 0f,
    startHpCounter = 0f,
    endHpCounter = 0f,
    dailyUsage = Nil
  )

  implicit val encoder: Encoder[PowerConsumption] =
    deriveEncoder[PowerConsumption]
  implicit def entityEncoder[F[_]]: EntityEncoder[F, PowerConsumption] =
    jsonEncoderOf[PowerConsumption]
}
