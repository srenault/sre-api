package sre.api.finance.models

import java.time.{LocalDate, YearMonth}

import cats.effect._
import io.circe.Encoder
import io.circe.generic.semiauto._
import org.http4s.EntityEncoder
import org.http4s.circe._
import sre.api.finance.analytics._

case class Period(
  startDate: LocalDate,
  endDate: Option[LocalDate],
  yearMonth: Option[YearMonth],
  result: Option[Long],
  balance: Option[Long]
)

object Period {

  private def round(d: Double): Long = {
    BigDecimal(d).setScale(0, BigDecimal.RoundingMode.HALF_UP).toLong
  }

  def apply(periodIndex: PeriodIndex): Period =
    Period(
      periodIndex.startDate,
      Some(periodIndex.endDate),
      Some(periodIndex.yearMonth),
      periodIndex.result.map(round),
      periodIndex.balance.map(round)
    )

  def apply(periodIndex: AnalyticsPeriodIndex): Period =
    Period(
      periodIndex.startDate,
      periodIndex.maybeEndDate,
      periodIndex.maybeYearMonth,
      periodIndex.result.map(round),
      periodIndex.balance.map(round)
    )

  implicit val encoder: Encoder[Period] = deriveEncoder[Period]
  implicit def entityEncoder[F[_]: Effect]: EntityEncoder[F, Period] = jsonEncoderOf[F, Period]
  implicit def entitiesEncoder[F[_]: Effect]: EntityEncoder[F, List[Period]] = jsonEncoderOf[F, List[Period]]
}
