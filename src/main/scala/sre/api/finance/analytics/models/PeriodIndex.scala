package sre.api.finance
package analytics

import java.util.Base64
import java.time.{ LocalDate, YearMonth }
import io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import anorm._
import anorm.SqlParser._
import cats.data.NonEmptyList
import ofx.OfxFile
import cm.CMStatement

sealed trait PeriodIndex {
  def partitions: List[OfxFile]
  def startDate: LocalDate
  def maybeEndDate: Option[LocalDate]
  def wageStatements: NonEmptyList[CMStatement]
  def result: Double
  def maybeYearMonth: Option[YearMonth]

  def startWageStatement: CMStatement =
    wageStatements.toList.sortBy(_.date.toEpochDay).head

  def includeStatements(statements: List[CMStatement], partitions: List[OfxFile] = Nil): PeriodIndex = {
    val amount = statements.foldLeft(0D)(_ + _.amount)

    this match {
      case period: CompletePeriodIndex =>
        period.copy(
          partitions = (period.partitions ++ partitions).distinct,
          result = result + amount
        )

      case period: IncompletePeriodIndex =>
        period.copy(
          partitions = (period.partitions ++ partitions).distinct,
          result = result + amount
        )
    }
  }

  def includeNewWageStatement(wageStatement: CMStatement, statements: List[CMStatement], partitions: List[OfxFile]): PeriodIndex = {
    val amount = statements.foldLeft(0D)(_ + _.amount)
    val updatedWageStatements = wageStatement :: wageStatements

    this match {
      case period: CompletePeriodIndex =>
        period.copy(
          partitions = (period.partitions ++ partitions).distinct,
          startDate = wageStatement.date,
          wageStatements = updatedWageStatements,
          result = result + amount
        )

      case period: IncompletePeriodIndex =>
        period.copy(
          partitions = (period.partitions ++ partitions).distinct,
          startDate = wageStatement.date,
          wageStatements = updatedWageStatements,
          result = result + amount
        )
    }
  }
}

object PeriodIndex {

  lazy val ORDER_ASC: scala.math.Ordering[PeriodIndex] =
    scala.math.Ordering.by[PeriodIndex, Long](_.startDate.toEpochDay)

  lazy val ORDER_DESC: scala.math.Ordering[PeriodIndex] =
    ORDER_ASC.reverse
}

case class CompletePeriodIndex(
  yearMonth: YearMonth,
  partitions: List[OfxFile],
  startDate: LocalDate,
  endDate: LocalDate,
  wageStatements: NonEmptyList[CMStatement],
  result: Double
) extends PeriodIndex {
  def maybeEndDate = Some(endDate)

  def maybeYearMonth = Some(yearMonth)
}

object CompletePeriodIndex {

  def apply(
    partitions: List[OfxFile],
    startDate: LocalDate,
    endDate: LocalDate,
    wageStatements: NonEmptyList[CMStatement]
  ): CompletePeriodIndex = {
    val yearMonth = computeYearMonth(startDate, endDate)
    CompletePeriodIndex(yearMonth, partitions, startDate, endDate, wageStatements, result = 0)
  }

  private val SEP = '|'

  private val ENCODING = "UTF-8"

  private def encodeBase64(s: String): String =
    Base64.getEncoder().encodeToString(s.getBytes(ENCODING))

  private def decodeBase64(s: String): String =
    new String(Base64.getDecoder().decode(s), ENCODING)

  def encodePartitions(partitions: List[OfxFile]): String =
    partitions.map(p => encodeBase64(p.file.getPath)).mkString(SEP.toString)

  def decodePartitions(partitionsStr: String): List[OfxFile] = {
    partitionsStr.split(SEP).toList.map { p =>
      OfxFile.open(decodeBase64(p)) match {
        case Some(file) => file
        case None => sys.error(s"Unable to find partition file $p")
      }
    }
  }

  def encodeWageStatements(wageStatements: NonEmptyList[CMStatement]): String = {
    wageStatements.map(wageStatement => encodeBase64(wageStatement.asJson.noSpaces)).toList.mkString(SEP.toString)
  }

  def decodeWageStatements(wageStatementsStr: String): List[CMStatement] = {
    wageStatementsStr.split(SEP).toList.map { s =>
      decode[CMStatement](decodeBase64(s)) match {
        case Left(err) => sys.error(s"Unable to decode $s as CMStatement")
        case Right(st) => st
      }
    }.toList
  }

  def computeYearMonth(startDate: LocalDate, endDate: LocalDate): YearMonth = {
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

  implicit lazy val parser: RowParser[CompletePeriodIndex] = (
    get[LocalDate]("yearMonth") ~
      get[LocalDate]("startdate") ~
      get[LocalDate]("enddate") ~
      get[String]("partitions") ~
      get[String]("wagestatements") ~
      get[Double]("result")
  ) map {
    case yearMonthDate ~ startDate ~ endDate ~ partitionsStr ~ wageStatementsStr ~ result =>
      val partitions = decodePartitions(partitionsStr)
      val wageStatements = decodeWageStatements(wageStatementsStr)
      NonEmptyList.fromList(wageStatements) match {
        case Some(nonEmptyWageStatements) =>
          val yearMonth = YearMonth.from(yearMonthDate)
          CompletePeriodIndex(yearMonth, partitions, startDate, endDate, nonEmptyWageStatements, result)

        case None =>
          sys.error(s"Invalid period index $yearMonthDate: wageStatements is empty")
      }

  }
}

case class IncompletePeriodIndex(
  partitions: List[OfxFile],
  startDate: LocalDate,
  wageStatements: NonEmptyList[CMStatement],
  result: Double
) extends PeriodIndex {
  def maybeEndDate = None

  def maybeYearMonth = None
}

object IncompletePeriodIndex {

  def apply(
    partitions: List[OfxFile],
    startDate: LocalDate,
    wageStatements: NonEmptyList[CMStatement]
  ): IncompletePeriodIndex = {
    IncompletePeriodIndex(partitions, startDate, wageStatements, result = 0)
  }
}
