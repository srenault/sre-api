package sre.api.finance

import cats.effect._
import cats.implicits._
import fs2.Stream
import java.time._
import java.sql._
import anorm._
import analytics.{ PeriodIndex, CompletePeriodIndex }

case class DBClient[F[_]]()(implicit connection: Connection, F: Sync[F]) {

  import DBClient.Ordering

  private def utc(period: YearMonth): ZonedDateTime = {
    period.atDay(1).atStartOfDay(ZoneOffset.UTC)
  }

  private def utc(date: LocalDate): ZonedDateTime = {
    date.atStartOfDay(ZoneOffset.UTC)
  }

  private def utc(dateTime: LocalDateTime): ZonedDateTime = {
    dateTime.atZone(ZoneOffset.UTC)
  }

  def upsertPeriodIndexes(periodIndexes: List[PeriodIndex]): F[Unit] =
    F.blocking {
      periodIndexes.collect {
        case period@CompletePeriodIndex(yearMonth, partitions, startDate, endDate, wageStatements, result, balancesByAccount) =>
          val encodedPartitions = CompletePeriodIndex.encodePartitions(partitions)
          val encodedWageStatements = CompletePeriodIndex.encodeWageStatements(wageStatements)
          val lastUpdate = java.time.LocalDateTime.now()
          val encodedBalancesByAccount = CompletePeriodIndex.encodeBalancesByAccount(balancesByAccount)

          SQL"""REPLACE INTO FINANCE_PERIODINDEX(yearmonth, startdate, enddate, partitions, wagestatements, result, balancesByAccount, lastupdate)
            values (
              ${utc(yearMonth)},
              ${utc(startDate)},
              ${utc(endDate)},
              $encodedPartitions,
              $encodedWageStatements,
              $result,
              $encodedBalancesByAccount,
              ${utc(lastUpdate)}
            )""".executeUpdate()
      }
    }

  def selectPeriodIndexes(maybeBeforePeriod: Option[YearMonth], maybeAfterPeriod: Option[YearMonth], limit: Int, yearMonthOrdering: Ordering.Value = Ordering.DESC): F[List[CompletePeriodIndex]] =
    F.blocking {
      try {
        (maybeBeforePeriod, maybeAfterPeriod) match {
          case (Some(beforePeriodDate), None) =>
            SQL(s"SELECT * FROM FINANCE_PERIODINDEX WHERE yearMonth < {yearmonth} ORDER BY yearmonth ${yearMonthOrdering} LIMIT {limit}")
              .on("yearmonth" -> utc(beforePeriodDate))
              .on("limit" -> limit)
              .as(CompletePeriodIndex.parser.*)

          case (None, Some(afterPeriodDate)) =>
            SQL(s"SELECT * FROM FINANCE_PERIODINDEX WHERE yearMonth > {yearmonth} ORDER BY yearmonth ${yearMonthOrdering} LIMIT {limit}")
              .on("yearmonth" -> utc(afterPeriodDate))
              .on("limit" -> limit)
              .as(CompletePeriodIndex.parser.*)

          case (Some(beforePeriodDate), Some(afterPeriodDate)) =>
            SQL(s"SELECT * FROM FINANCE_PERIODINDEX WHERE yearMonth > {yearmonthAfter} AND yearMonth < {yearmonthBefore} ORDER BY yearmonth ${yearMonthOrdering} LIMIT {limit}")
              .on("yearmonthBefore" -> utc(beforePeriodDate))
              .on("yearmonthAfter" -> utc(afterPeriodDate))
              .on("limit" -> limit)
              .as(CompletePeriodIndex.parser.*)

          case (None, None) =>
            SQL("SELECT * FROM FINANCE_PERIODINDEX ORDER BY yearmonth DESC LIMIT {limit}")
              .on("limit" -> limit)
              .as(CompletePeriodIndex.parser.*)
        }
      } catch {
        case e: Exception =>
          e.printStackTrace
          throw e
      }
    }

  def countPeriodIndexes(maybeBeforePeriod: Option[YearMonth] = None, maybeAfterPeriod: Option[YearMonth] = None): F[Long] =
    F.blocking {
      try {
        (maybeBeforePeriod, maybeAfterPeriod) match {
          case (Some(beforePeriodDate), None) =>
            SQL("SELECT COUNT(*) FROM FINANCE_PERIODINDEX WHERE yearMonth < {yearmonth}")
              .on("yearmonth" -> utc(beforePeriodDate))
              .as(SqlParser.scalar[Long].single)

          case (None, Some(afterPeriodDate)) =>
            SQL("SELECT COUNT(*) as COUNT FROM FINANCE_PERIODINDEX WHERE yearMonth > {yearmonth}")
              .on("yearmonth" -> utc(afterPeriodDate))
              .as(SqlParser.scalar[Long].single)

          case (Some(beforePeriodDate), Some(afterPeriodDate)) =>
            SQL("SELECT COUNT(*) as COUNT FROM FINANCE_PERIODINDEX WHERE yearMonth > {yearmonthAfter} AND yearMonth < {yearmonthBefore}")
              .on("yearmonthBefore" -> utc(beforePeriodDate))
              .on("yearmonthAfter" -> utc(afterPeriodDate))
              .as(SqlParser.scalar[Long].single)

          case (None, None) =>
            SQL("SELECT COUNT(*) FROM FINANCE_PERIODINDEX")
              .as(SqlParser.scalar[Long].single)
        }
      } catch {
        case e: Exception =>
          e.printStackTrace
          throw e
      }
    }

  def selectOnePeriodIndex(periodDate: YearMonth): F[Option[CompletePeriodIndex]] =
    F.blocking {
      try {
        SQL("SELECT * FROM FINANCE_PERIODINDEX WHERE yearmonth = {yearmonth}")
          .on("yearmonth" -> utc(periodDate.atDay(1)))
          .as(CompletePeriodIndex.parser.singleOpt)
      } catch {
        case e: Exception =>
          e.printStackTrace
          throw e
      }
    }
}

object DBClient {

  object Ordering extends Enumeration {
    val ASC,DESC = Value
  }

  private def init[F[_]]()(implicit connection: Connection, F: Sync[F]): F[Unit] =
    F.blocking {
      SQL"""CREATE TABLE IF NOT EXISTS FINANCE_PERIODINDEX (
            yearmonth NUMERIC PRIMARY_KEY,
            startdate NUMERIC UNIQUE,
            enddate NUMERIC UNIQUE,
            partitions TEXT,
            wagestatements TEXT,
            result NUMERIC,
            balancesByAccount TEXT,
            lastupdate NUMERIC
    );""".execute()
    }

  def resource[F[_]: Sync](settings: Settings): Resource[F, DBClient[F]] = {
    Class.forName("org.sqlite.JDBC")
    implicit val connection = DriverManager.getConnection(settings.db)
    val dbClient = DBClient()
    Resource.eval(init().map(_ => dbClient))
  }
}
