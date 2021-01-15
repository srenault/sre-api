package sre.api

import cats.effect._
import cats.implicits._
import fs2.Stream
import java.time.YearMonth
import java.sql.{Connection, DriverManager}

import anorm._
import finance.analytics.{AnalyticsPeriodIndex, CompleteAnalyticsPeriodIndex}
import sre.api.finance.analytics.PeriodIndex

case class DBClient[F[_]]()(implicit connection: Connection, F: Effect[F]) {

  def upsertPeriodIndexes(analyticsPeriodIndexes: List[AnalyticsPeriodIndex]): F[Unit] =
    F.delay {
      val periodIndexes = analyticsPeriodIndexes.collect {
        case p: CompleteAnalyticsPeriodIndex => PeriodIndex(p)
      }

      periodIndexes.map { periodIndex =>
        val lastUpdate = java.time.LocalDateTime.now()

        SQL"""REPLACE INTO FINANCE_PERIODINDEX(yearmonth, startdate, enddate, partitions, wagestatements, result, balance, lastupdate)
            values (
              ${periodIndex.yearMonth.atDay(1)},
              ${periodIndex.startDate},
              ${periodIndex.endDate},
              ${periodIndex.encodedPartitions},
              ${periodIndex.encodedWageStatements},
              ${periodIndex.result},
              ${periodIndex.balance},
              $lastUpdate
            )""".executeUpdate()
      }
    }

  def selectAllPeriodIndexes(): F[List[PeriodIndex]] =
    F.delay {
      try {
        SQL"SELECT * FROM FINANCE_PERIODINDEX".as(PeriodIndex.parser.*)
      } catch {
        case e: Exception =>
          e.printStackTrace
          throw e
      }
    }

  def selectOnePeriodIndex(periodDate: YearMonth): F[Option[PeriodIndex]] =
    F.delay {
      try {
        SQL("SELECT * FROM FINANCE_PERIODINDEX WHERE yearmonth = {yearmonth}")
          .on("yearmonth" -> periodDate.atDay(1))
          .as(PeriodIndex.parser.singleOpt)
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
            balance NUMERIC,
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
