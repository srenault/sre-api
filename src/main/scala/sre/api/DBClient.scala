package sre.api

import cats.effect._
import cats.implicits._
import fs2.Stream
import java.time.LocalDate
import java.sql.{ DriverManager, Connection }
import anorm._
import finance.analytics.PeriodIndex

case class DBClient[F[_]](implicit connection: Connection, F: Effect[F]) {

  def upsertPeriodIndexes(periodIndexes: List[PeriodIndex]): F[Unit] =
    F.delay {
      periodIndexes.collect {
        case PeriodIndex(partitions, startDate, Some(endDate), wageStatements, balance) =>
          val yearMonth = PeriodIndex.yearMonth(startDate, endDate)
          val encodedPartitions = PeriodIndex.encodePartitions(partitions)
          val encodedWageStatements = PeriodIndex.encodeWageStatements(wageStatements)
          val lastUpdate = java.time.LocalDateTime.now()

          SQL"""REPLACE INTO FINANCE_PERIODINDEX(yearmonth, startdate, enddate, partitions, wagestatements, balance, lastupdate)
            values ($yearMonth, $startDate, $endDate, $encodedPartitions, $encodedWageStatements, $balance, $lastUpdate)""".executeUpdate()
      }
    }

  def selectAllPeriodIndexes(): F[List[PeriodIndex]] = F.delay {
    try {
      SQL"SELECT * FROM FINANCE_PERIODINDEX".as(PeriodIndex.parser.*)
    } catch {
      case e: Exception =>
        e.printStackTrace
        throw e
    }
  }

  def selectOnePeriodIndex(date: LocalDate): F[Option[PeriodIndex]] = F.delay {
    try {
      SQL("SELECT * FROM FINANCE_PERIODINDEX WHERE startdate <= {date} and enddate > {date}")
        .on("date" -> date)
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
