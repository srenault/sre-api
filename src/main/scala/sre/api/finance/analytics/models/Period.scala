package sre.api.finance
package analytics

import java.time.LocalDate
import cats.effect._
import org.http4s.EntityEncoder
import org.http4s.circe._
import io.circe.Encoder
import io.circe.generic.semiauto._

case class Period(startDate: LocalDate, endDate: Option[LocalDate], balance: Long)

object Period {

  def apply(startDate: LocalDate, endDate: Option[LocalDate], balance: Double): Period = {
    val roundBalance = BigDecimal(balance).setScale(0, BigDecimal.RoundingMode.HALF_UP).toLong
    Period(startDate, endDate, roundBalance)
  }

  implicit val encoder: Encoder[Period] = deriveEncoder[Period]
  implicit def entityEncoder[F[_]: Effect]: EntityEncoder[F, Period] = jsonEncoderOf[F, Period]
  implicit def entitiesEncoder[F[_]: Effect]: EntityEncoder[F, List[Period]] = jsonEncoderOf[F, List[Period]]
}
