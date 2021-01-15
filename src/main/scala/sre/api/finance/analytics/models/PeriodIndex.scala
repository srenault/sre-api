package sre.api.finance.analytics

import java.time.{LocalDate, YearMonth}
import java.util.Base64

import anorm._
import anorm.SqlParser._
import cats.data.NonEmptyList
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import sre.api.finance.analytics.CompleteAnalyticsPeriodIndex
import sre.api.finance.cm.CMStatement
import sre.api.finance.ofx.OfxFile

case class PeriodIndex(
  yearMonth: YearMonth,
  partitions: List[OfxFile],
  startDate: LocalDate,
  endDate: LocalDate,
  wageStatements: NonEmptyList[CMStatement],
  result: Double,
  balance: Double
) {
  val encodedPartitions = PeriodIndex.encodePartitions(partitions)
  val encodedWageStatements = PeriodIndex.encodeWageStatements(wageStatements)
}

object PeriodIndex {

  def computeResult(): Double = ???

  def computeBalance(): Double = ???

  def apply(analyticsPeriodIndex: CompleteAnalyticsPeriodIndex): PeriodIndex = {
   PeriodIndex(
     analyticsPeriodIndex.yearMonth,
     analyticsPeriodIndex.partitions,
     analyticsPeriodIndex.startDate,
     analyticsPeriodIndex.endDate,
     analyticsPeriodIndex.wageStatements,
     computeResult(),
     computeBalance()
   )
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
  implicit lazy val parser: RowParser[PeriodIndex] = (
      get[LocalDate]("yearMonth") ~
      get[LocalDate]("startdate") ~
      get[LocalDate]("enddate") ~
      get[String]("partitions") ~
      get[String]("wagestatements") ~
      get[Double]("result") ~
      get[Double]("amount")
    ) map {
    case yearMonthDate ~ startDate ~ endDate ~ partitionsStr ~ wageStatementsStr ~ balance ~ amount =>
      val partitions = decodePartitions(partitionsStr)
      val wageStatements = decodeWageStatements(wageStatementsStr)
      NonEmptyList.fromList(wageStatements) match {
        case Some(nonEmptyWageStatements) =>
          val yearMonth = YearMonth.from(yearMonthDate)
          PeriodIndex(
            yearMonth,
            partitions,
            startDate,
            endDate,
            nonEmptyWageStatements,
            balance,
            amount
          )

        case None =>
          sys.error(s"Invalid period index $yearMonthDate: wageStatements is empty")
      }

  }
}
