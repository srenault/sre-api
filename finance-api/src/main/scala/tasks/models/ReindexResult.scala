package sre.api.finance
package tasks.models

import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import analytics.PeriodIndex

final case class ReindexResult(periods: List[PeriodIndex])

object ReindexResult {
    implicit val encoderPeriods: Encoder[List[PeriodIndex]] = new Encoder[List[PeriodIndex]] {
        final def apply(periods: List[PeriodIndex]): Json = {
            periods.map { period =>
                val endDate = period.maybeEndDate.getOrElse("N/A")
                s"${period.startDate} - $endDate"
            }.asJson
        }
    }
    implicit val encoder: Encoder[ReindexResult] = deriveEncoder[ReindexResult]
}