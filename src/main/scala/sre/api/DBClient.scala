package sre.api

import cats.effect._
import cats.implicits._
import fs2.Stream
import java.time.YearMonth
import java.sql.{ DriverManager, Connection }
import anorm._
import finance.analytics.{ PeriodIndex, CompletePeriodIndex }

case class DBClient[F[_]]()(implicit connection: Connection, F: Effect[F]) {

  def upsertPeriodIndexes(periodIndexes: List[PeriodIndex]): F[Unit] =
    F.delay {
      periodIndexes.collect {
        case period@CompletePeriodIndex(yearMonth, partitions, startDate, endDate, wageStatements, result, balancesByAccount) =>
          val encodedPartitions = CompletePeriodIndex.encodePartitions(partitions)
          val encodedWageStatements = CompletePeriodIndex.encodeWageStatements(wageStatements)
          val lastUpdate = java.time.LocalDateTime.now()
          val encodedBalancesByAccount = CompletePeriodIndex.encodeBalancesByAccount(balancesByAccount)

          SQL"""REPLACE INTO FINANCE_PERIODINDEX(yearmonth, startdate, enddate, partitions, wagestatements, result, balancesByAccount, lastupdate)
            values (
              ${yearMonth.atDay(1)},
              $startDate,
              $endDate,
              $encodedPartitions,
              $encodedWageStatements,
              $result,
              $encodedBalancesByAccount,
              $lastUpdate
            )""".executeUpdate()
      }
    }

  def selectPeriodIndexes(maybeBeforePeriod: Option[YearMonth], limit: Int): F[List[CompletePeriodIndex]] =
    F.delay {
      try {
        maybeBeforePeriod match {
          case Some(beforePeriodDate) =>
            SQL("SELECT * FROM FINANCE_PERIODINDEX WHERE yearMonth < {yearmonth} ORDER BY yearmonth DESC LIMIT {limit}")
              .on("yearmonth" -> beforePeriodDate.atDay(1))
              .on("limit" -> limit)
              .as(CompletePeriodIndex.parser.*)

          case None =>
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

  def selectOnePeriodIndex(periodDate: YearMonth): F[Option[CompletePeriodIndex]] =
    F.delay {
      try {
        SQL("SELECT * FROM FINANCE_PERIODINDEX WHERE yearmonth = {yearmonth}")
          .on("yearmonth" -> periodDate.atDay(1))
          .as(CompletePeriodIndex.parser.singleOpt)
      } catch {
        case e: Exception =>
          e.printStackTrace
          throw e
      }
    }
}

object DBClient {

  def init[F[_]]()(implicit connection: Connection, F: Effect[F]): F[Unit] =
    F.delay {
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

  def stream[F[_]: Effect](settings: Settings): Stream[F, DBClient[F]] = {
    Class.forName("org.sqlite.JDBC")
    implicit val connection = DriverManager.getConnection(settings.db)
    val dbClient = DBClient()
    Stream.eval(init().map(_ => dbClient))
  }
}
