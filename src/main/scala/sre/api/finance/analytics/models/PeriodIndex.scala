package sre.api.finance
package analytics

import java.util.Base64
import java.time.LocalDate
import io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import anorm._
import anorm.SqlParser._
import ofx.OfxFile
import cm.CMStatement

case class PeriodIndex(
  partitions: List[OfxFile],
  startDate: LocalDate,
  endDate: Option[LocalDate],
  wageStatements: List[CMStatement],
  balance: Double
)

object PeriodIndex {

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

  def encodeWageStatements(wageStatements: List[CMStatement]): String = {
    wageStatements.map(wageStatement => encodeBase64(wageStatement.asJson.noSpaces)).mkString(SEP.toString)
  }

  def decodeWageStatements(wageStatementsStr: String): List[CMStatement] = {
    wageStatementsStr.split(SEP).toList.map { s =>
      decode[CMStatement](decodeBase64(s)) match {
        case Left(err) => sys.error(s"Unable to decode $s as CMStatement")
        case Right(st) => st
      }
    }.toList
  }

  def yearMonth(startDate: LocalDate, endDate: LocalDate): LocalDate = {
    if (startDate.getMonth == endDate.getMonth) {
      startDate.withDayOfMonth(1)
    } else {
      val periodA = startDate.lengthOfMonth - startDate.getDayOfMonth
      val periodB = endDate.getDayOfMonth

      if (periodA > periodB)  {
        startDate.withDayOfMonth(1)
      } else {
        endDate.withDayOfMonth(1)
      }
    }
  }

  implicit lazy val parser: RowParser[PeriodIndex] = (
      get[LocalDate]("startdate") ~
      get[LocalDate]("enddate") ~
      get[String]("partitions") ~
      get[String]("wagestatements") ~
      get[Double]("balance")
    ) map {
      case startDate ~ endDate ~ partitionsStr ~ wageStatementsStr ~ balance =>
      val partitions = PeriodIndex.decodePartitions(partitionsStr)
      val wageStatements = PeriodIndex.decodeWageStatements(wageStatementsStr)
      PeriodIndex(partitions, startDate, Option(endDate), wageStatements, balance)
    }
}
