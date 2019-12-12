package sre.api.finance
package analytics

import java.time.{ LocalDate, YearMonth }
import cats.effect._
import org.http4s.EntityEncoder
import org.http4s.circe._
import io.circe.Encoder
import io.circe.generic.semiauto._

case class Period(startDate: LocalDate, endDate: Option[LocalDate], balance: Long)

object Period {

  def apply(
    startDate: LocalDate,
    endDate: Option[LocalDate],
    yearMonth: Option[YearMonth],
    balance: Double
  ): Period = {
    val roundBalance = BigDecimal(balance).setScale(0, BigDecimal.RoundingMode.HALF_UP).toLong
    Period(startDate, endDate, roundBalance)
  }

  def apply(periodIndex: PeriodIndex): Period =
    Period(
      periodIndex.startDate,
      periodIndex.maybeEndDate,
      periodIndex.maybeYearMonth,
      periodIndex.balance
    )

  implicit val encoder: Encoder[Period] = deriveEncoder[Period]
  implicit def entityEncoder[F[_]: Effect]: EntityEncoder[F, Period] = jsonEncoderOf[F, Period]
  implicit def entitiesEncoder[F[_]: Effect]: EntityEncoder[F, List[Period]] = jsonEncoderOf[F, List[Period]]
}
