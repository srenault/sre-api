package sre.api.finance
package analytics

import cats.implicits._
import java.time.LocalDate
import ofx.OfxFile
import cm.CMStatement

case class SegmentIndex(
  partitions: Seq[OfxFile],
  wageStatement: Option[CMStatement],
  statements: List[CMStatement]
) {
  lazy val sortedStatements = statements.sortBy(_.date.toEpochDay)

  lazy val startDate: LocalDate = sortedStatements.head.date

  lazy val endDate: LocalDate = sortedStatements.last.date
}
