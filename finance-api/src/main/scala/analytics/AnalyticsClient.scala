package sre.api.finance
package analytics

import java.time.YearMonth
import cats.data.OptionT
import cats.effect._
import cats.implicits._
import cm.{ CMAccountState, CMStatement }

case class AnalyticsClient[F[_]](
  indexClient: AnalyticsIndexClient[F],
  dbClient: DBClient[F],
  settings: Settings
)(implicit F: Sync[F]) {

  def reindex(fromScratch: Boolean): F[List[PeriodIndex]] = {
    val eventuallyIndexes = if (fromScratch) {
      indexClient.computePeriodIndexesFromScratch()
    } else {
      indexClient.computeLatestPeriodIndexes()
    }

    eventuallyIndexes.flatMap { indexes =>
      dbClient.upsertPeriodIndexes(indexes).map(_ => indexes)
    }
  }

  def getStatementsForPeriod(periodDate: YearMonth): OptionT[F, (Period, List[CMStatement])] = {
    for {
      periodIndex <- OptionT(dbClient.selectOnePeriodIndex(periodDate))

      statements <- OptionT.liftF {
        periodIndex.partitions
          .map(f => ofx.OfxStmTrn.load(f).map(f.accountId -> _))
          .sequence
          .map { transactionsByAccount =>
            CMStatement.merge(transactionsByAccount.map {
              case (accountId, transactions) =>
                transactions.map(_.toStatement(accountId))
            }.flatten).sorted(CMStatement.ORDER_ASC)
             .dropWhile(_.date.isBefore(periodIndex.startDate))
             .takeWhile(_.date.isBefore(periodIndex.endDate))
          }
      }
    } yield Period(periodIndex) -> statements
  }

  def getAccountStateForPeriod(accountId: String, periodDate: YearMonth): OptionT[F, (Period, CMAccountState)] = {
    for {
      accountSettings <- OptionT(F.pure(settings.finance.cm.accounts.find(_.id == accountId)))

      periodIndex <- OptionT(dbClient.selectOnePeriodIndex(periodDate))

      statements <- OptionT.liftF {
        periodIndex.partitions
          .filter(_.accountId == accountId)
          .map(f => ofx.OfxStmTrn.load(f))
          .sequence.map { ofxStmTrn =>
            CMStatement.merge(
              ofxStmTrn
                .flatten
                .map(_.toStatement(accountId))
            ).sorted(CMStatement.ORDER_ASC)
              .dropWhile(_.date.isBefore(periodIndex.startDate))
              .takeWhile(_.date.isBefore(periodIndex.endDate))
          }
      }
    } yield {
      Period(periodIndex) -> CMAccountState(accountSettings, statements)
    }
  }

  def countPeriods(maybeBeforePeriod: Option[YearMonth] = None, maybeAfterPeriod: Option[YearMonth] = None): F[Long] = {
    dbClient.countPeriodIndexes(maybeBeforePeriod, maybeAfterPeriod)
  }

  def getPeriods(maybeBeforePeriod: Option[YearMonth], maybeAfterPeriod: Option[YearMonth], limit: Int): F[List[Period]] = {
    val ordering = if (maybeBeforePeriod.isDefined) DBClient.Ordering.DESC else DBClient.Ordering.ASC

    dbClient.selectPeriodIndexes(maybeBeforePeriod, maybeAfterPeriod, limit, ordering).map { periodIndexes =>
      periodIndexes.map(Period(_)).sortBy(-_.startDate.toEpochDay)
    }
  }

  def computeCurrentPeriod(statements: List[CMStatement]): Option[Period] = {
    val indexes = indexClient.computePeriodIndexesFrom(statements)
    indexes.lastOption.map(Period(_))
  }
}

object AnalyticsClient {
  def apply[F[_]](
    dbClient: DBClient[F],
    settings: Settings
  )(implicit F: Sync[F]): AnalyticsClient[F] = {
    val indexClient = AnalyticsIndexClient(dbClient, settings)
    AnalyticsClient(indexClient, dbClient, settings)
  }
}
