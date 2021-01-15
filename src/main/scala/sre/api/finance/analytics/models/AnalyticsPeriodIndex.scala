package sre.api.finance
package analytics

import java.time.{LocalDate, YearMonth}

import cats.data.NonEmptyList
import ofx.OfxFile
import cm.CMStatement

sealed trait AnalyticsPeriodIndex {
  def partitions: List[OfxFile]
  def startDate: LocalDate
  def maybeEndDate: Option[LocalDate]
  def maybeYearMonth: Option[YearMonth]
  def wageStatements: NonEmptyList[CMStatement]
  def result: Double
  def maybeBalance: Option[Double]

  def startWageStatement: CMStatement =
    wageStatements.toList.sortBy(_.date.toEpochDay).head

  def includeStatements(statements: List[CMStatement], partitions: List[OfxFile] = Nil): AnalyticsPeriodIndex = {
    val amount = statements.foldLeft(0D)(_ + _.amount)

    this match {
      case period: CompleteAnalyticsPeriodIndex =>
        period.copy(
          partitions = (period.partitions ++ partitions).distinct,
          result = result + amount
        )

      case period: IncompleteAnalyticsPeriodIndex =>
        period.copy(
          partitions = (period.partitions ++ partitions).distinct,
          result = result + amount
        )
    }
  }

  def includeNewWageStatement(wageStatement: CMStatement, statements: List[CMStatement], partitions: List[OfxFile]): AnalyticsPeriodIndex = {
    val updatedWageStatements = wageStatement :: wageStatements

    this match {
      case period: CompleteAnalyticsPeriodIndex =>
        period.copy(
          partitions = (period.partitions ++ partitions).distinct,
          startDate = wageStatement.date,
          wageStatements = updatedWageStatements
        )

      case period: IncompleteAnalyticsPeriodIndex =>
        period.copy(
          partitions = (period.partitions ++ partitions).distinct,
          startDate = wageStatement.date,
          wageStatements = updatedWageStatements,
        )
    }
  }
}

object AnalyticsPeriodIndex {

  lazy val ORDER_ASC: scala.math.Ordering[AnalyticsPeriodIndex] =
    scala.math.Ordering.by[AnalyticsPeriodIndex, Long](_.startDate.toEpochDay)

  lazy val ORDER_DESC: scala.math.Ordering[AnalyticsPeriodIndex] =
    ORDER_ASC.reverse

  def computeYearMonth(startDate: LocalDate, endDate: LocalDate): YearMonth = {
    if (startDate.getMonth == endDate.getMonth) {
      YearMonth.from(startDate)
    } else {
      val periodA = startDate.lengthOfMonth - startDate.getDayOfMonth
      val periodB = endDate.getDayOfMonth

      if (periodA > periodB)  {
        YearMonth.from(startDate)
      } else {
        YearMonth.from(endDate)
      }
    }
  }
}

case class CompleteAnalyticsPeriodIndex(
  partitions: List[OfxFile],
  startDate: LocalDate,
  endDate: LocalDate,
  wageStatements: NonEmptyList[CMStatement],
  result: Double,
  balance: Double
) extends AnalyticsPeriodIndex {
  lazy val yearMonth: YearMonth = AnalyticsPeriodIndex.computeYearMonth(startDate, endDate)

  lazy val maybeYearMonth: Option[YearMonth] = Some(yearMonth)

  val maybeEndDate: Option[LocalDate] = Some(endDate)

  val maybeBalance: Option[Double] = Some(balance)
}

object CompleteAnalyticsPeriodIndex {

  def apply(
    partitions: List[OfxFile],
    startDate: LocalDate,
    endDate: LocalDate,
    wageStatements: NonEmptyList[CMStatement],
  ): CompleteAnalyticsPeriodIndex = {
    CompleteAnalyticsPeriodIndex(
      partitions,
      startDate,
      endDate,
      wageStatements,
      result = 0,
      balance = 0
    )
  }
}

case class IncompleteAnalyticsPeriodIndex(
  partitions: List[OfxFile],
  startDate: LocalDate,
  wageStatements: NonEmptyList[CMStatement],
  result: Double
) extends AnalyticsPeriodIndex {
  lazy val maybeYearMonth: Option[YearMonth] = None

  lazy val maybeEndDate: Option[LocalDate] = None

  lazy val maybeBalance: Option[Double] = None
}

object IncompleteAnalyticsPeriodIndex {
  def apply(
    partitions: List[OfxFile],
    startDate: LocalDate,
    wageStatements: NonEmptyList[CMStatement]
  ): IncompleteAnalyticsPeriodIndex = {
    IncompleteAnalyticsPeriodIndex(
      partitions,
      startDate,
      wageStatements,
      result = 0
    )
  }
}