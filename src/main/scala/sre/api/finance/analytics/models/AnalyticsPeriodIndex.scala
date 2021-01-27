package sre.api.finance
package analytics

import java.time.temporal.ChronoUnit
import java.time._

import cats.data.NonEmptyList
import ofx.OfxFile
import cm.CMStatement

sealed trait AnalyticsPeriodIndex {

  def partitions: List[OfxFile]
  def startDate: LocalDate
  def maybeEndDate: Option[LocalDate]
  def maybeYearMonth: Option[YearMonth]
  def wageStatements: NonEmptyList[CMStatement]
  def result: Option[Double]
  def balance: Option[Double]

  def startWageStatement: CMStatement =
    wageStatements.toList.sortBy(_.date.toEpochDay).head

  def includeStatements(statements: List[CMStatement], newPartitions: List[OfxFile] = Nil): AnalyticsPeriodIndex

  def includeNewWageStatement(
    wageStatement: CMStatement,
    statements: List[CMStatement],
    newPartitions: List[OfxFile]
  ): AnalyticsPeriodIndex

  def isValid: Boolean = {
    val endDate = maybeEndDate getOrElse LocalDate.now()
    val nbDays = ChronoUnit.DAYS.between(startDate, endDate)
    nbDays < 40
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
      val period = List.unfold(startDate) {
        case d if d.isBefore(endDate) || d.isEqual(endDate) =>
          val nextDay = d.plusDays(1)
          Some(nextDay -> nextDay)

        case _ =>
          None
      }

      val (_, dates) = period.groupBy(_.getMonth).maxBy {
        case (_, dates) => dates.length
      }

      YearMonth.from(dates.head)
    }
  }

  def computeResult(maybeResult: Option[Double], statements: List[CMStatement]): Option[Double] = {
    val maybeNewResult = if (statements.nonEmpty) {
      Some(statements.foldLeft(0D)(_ + _.amount))
    } else None

    maybeNewResult.fold(maybeResult) { newResult =>
      Some(maybeResult.getOrElse(0D) + newResult)
    }
  }
}

case class CompleteAnalyticsPeriodIndex(
  partitions: List[OfxFile],
  startDate: LocalDate,
  endDate: LocalDate,
  wageStatements: NonEmptyList[CMStatement],
  result: Option[Double],
  balance: Option[Double]
) extends AnalyticsPeriodIndex {
  lazy val yearMonth: YearMonth = AnalyticsPeriodIndex.computeYearMonth(startDate, endDate)

  lazy val maybeYearMonth: Option[YearMonth] = Some(yearMonth)

  val maybeEndDate: Option[LocalDate] = Some(endDate)

  def includeStatements(statements: List[CMStatement], newPartitions: List[OfxFile] = Nil): CompleteAnalyticsPeriodIndex = {
    val updatedResult = AnalyticsPeriodIndex.computeResult(result, statements)

    val lastStatement = statements.filter {
      statement => maybeEndDate.exists(date => statement.date.isEqual(date))
    }.headOption

    this.copy(
      partitions = (partitions ++ newPartitions).distinct,
      result = updatedResult,
      balance = balance orElse lastStatement.map(_.balance)
    )
  }

  def includeNewWageStatement(
    wageStatement: CMStatement,
    statements: List[CMStatement],
    newPartitions: List[OfxFile]
  ): CompleteAnalyticsPeriodIndex = {
    this.copy(
      partitions = (partitions ++ newPartitions).distinct,
      wageStatements = wageStatement :: wageStatements
    )
  }
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
      result = None,
      balance = None
    )
  }
}

case class IncompleteAnalyticsPeriodIndex(
  partitions: List[OfxFile],
  startDate: LocalDate,
  wageStatements: NonEmptyList[CMStatement],
  result: Option[Double],
  balance: Option[Double]
) extends AnalyticsPeriodIndex {
  lazy val maybeYearMonth: Option[YearMonth] = None

  lazy val maybeEndDate: Option[LocalDate] = None

  def includeStatements(statements: List[CMStatement], newPartitions: List[OfxFile] = Nil): IncompleteAnalyticsPeriodIndex = {
    val updatedResult = AnalyticsPeriodIndex.computeResult(result, statements)

    val lastStatement = statements.filter {
      statement => maybeEndDate.exists(date => statement.date.isEqual(date))
    }.headOption

    this.copy(
      partitions = (partitions ++ newPartitions).distinct,
      result = updatedResult,
      balance = balance orElse lastStatement.map(_.balance)
    )
  }

  def includeNewWageStatement(
    wageStatement: CMStatement,
    statements: List[CMStatement],
    newPartitions: List[OfxFile]
  ): IncompleteAnalyticsPeriodIndex = {
    this.copy(
      partitions = (partitions ++ newPartitions).distinct,
      wageStatements = wageStatement :: wageStatements
    )
  }
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
      result = None,
      balance = None
    )
  }
}
