package sre.api.finance
package analytics

import java.time.{LocalDate, YearMonth}
import cats.effect._
import org.http4s.EntityEncoder
import org.http4s.circe._
import io.circe.Encoder
import io.circe.generic.semiauto._

case class Period(
    startDate: LocalDate,
    endDate: Option[LocalDate],
    yearMonth: Option[YearMonth],
    result: Long,
    balancesByAccount: Map[String, Long]
)

object Period {

  def round(n: Double): Long =
    BigDecimal(n).setScale(0, BigDecimal.RoundingMode.HALF_UP).toLong

  def apply(
      startDate: LocalDate,
      endDate: Option[LocalDate],
      yearMonth: Option[YearMonth],
      result: Double,
      balancesByAccount: Map[String, Double]
  ): Period = {
    Period(
      startDate,
      endDate,
      yearMonth,
      round(result),
      balancesByAccount.view.mapValues(round).toMap
    )
  }

  def apply(periodIndex: PeriodIndex): Period =
    Period(
      periodIndex.startDate,
      periodIndex.maybeEndDate,
      periodIndex.maybeYearMonth,
      periodIndex.result,
      periodIndex.balancesByAccount
    )

  implicit val encoder: Encoder[Period] = deriveEncoder[Period]
  implicit def entityEncoder[F[_]: Sync]: EntityEncoder[F, Period] =
    jsonEncoderOf[F, Period]
  implicit def entitiesEncoder[F[_]: Sync]: EntityEncoder[F, List[Period]] =
    jsonEncoderOf[F, List[Period]]
}
