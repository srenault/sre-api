package sre.api.finance
package analytics

import cats.implicits._
import java.time.LocalDate
import ofx.OfxFile
import cm.CMStatement

case class SegmentIndex(
  partitions: Seq[OfxFile],
  wageStatement: Option[CMStatement],
  statements: List[CMStatement] //TODO
) {
  lazy val sortedStatements = statements.sortBy(_.date.toEpochDay)

  lazy val startDate: LocalDate = sortedStatements.head.date

  lazy val endDate: LocalDate = sortedStatements.last.date
}

object SegmentIndex {

  lazy val ORDER_ASC: scala.math.Ordering[SegmentIndex] =
    scala.math.Ordering.by[SegmentIndex, Long](_.startDate.toEpochDay)

  lazy val ORDER_DESC: scala.math.Ordering[SegmentIndex] =
    ORDER_ASC.reverse
}
