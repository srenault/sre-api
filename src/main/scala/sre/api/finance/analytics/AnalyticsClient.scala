package sre.api
package finance
package analytics

import java.time.YearMonth
import cats.data.OptionT
import cats.effect._
import cats.implicits._
import cm.{ CMAccountState, CMStatement }
import icompta.IComptaClient

case class AnalyticsClient[F[_]](
  indexClient: AnalyticsIndexClient[F],
  icomptaClient: IComptaClient[F],
  dbClient: DBClient[F],
  settings: Settings
)(implicit F: Effect[F]) {

  def reindex(fromScratch: Boolean): F[List[PeriodIndex]] = {
    val eventuallyIndexes = if (fromScratch) {
      indexClient.computePeriodIndexesFromScrach()
    } else {
      indexClient.computeLatestPeriodIndexes()
    }

    eventuallyIndexes.flatMap { indexes =>
      dbClient.upsertPeriodIndexes(indexes)
    } *> eventuallyIndexes
  }

  def computeExpensesByCategory(account: CMAccountState): OptionT[F, List[ExpensesByCategory]] = {
    OptionT.liftF(icomptaClient.buildRulesAst().map { rulesAst =>
      val categoriesSettings = settings.finance.cm.accounts
        .find(_.id == account.id)
        .map(_.categories)
        .getOrElse(Map.empty)

      categoriesSettings.flatMap {
        case (categoryId, categorySettings) =>
          rulesAst.traverse(categorySettings.rulePath) map { ruleAst =>
            val amount = account.statements.foldLeft(0F) { (acc, statement) =>
              if (ruleAst.test(statement)) {
                acc + scala.math.abs(statement.amount)
              } else {
                acc
              }
            }
            ExpensesByCategory(categoryId, categorySettings.label, amount, categorySettings.threshold)
          }
      }.toList
    })
  }

  def getStatementsForPeriod(periodDate: YearMonth): OptionT[F, (Period, List[CMStatement])] = {
    for {
      periodIndex <- OptionT(dbClient.selectOnePeriodIndex(periodDate))

      statements <- OptionT.liftF {
        periodIndex.partitions
          .map(f => finance.ofx.OfxStmTrn.load(f).map(f.accountId -> _))
          .sequence
          .map { transactionsByAccount =>
            transactionsByAccount.map {
              case (accountId, transactions) =>
                transactions.map(_.toStatement(accountId))
            }.flatten.distinct.sorted(CMStatement.ORDER_ASC)
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
          .map(f => finance.ofx.OfxStmTrn.load(f))
          .sequence.map { ofxStmTrn =>
            ofxStmTrn
              .flatten
              .map(_.toStatement(accountId))
              .distinct
              .sorted(CMStatement.ORDER_ASC)
              .dropWhile(_.date.isBefore(periodIndex.startDate))
              .takeWhile(_.date.isBefore(periodIndex.endDate))
          }
      }
    } yield {
      Period(periodIndex) -> CMAccountState(accountSettings, statements)
    }
  }

  def getPreviousPeriods(): F[List[Period]] = {
    dbClient.selectAllPeriodIndexes().map { periodIndexes =>
      periodIndexes.map { periodIndex =>
        Period(periodIndex)
      }
    }
  }

  def computeCurrentPeriod(statements: List[CMStatement]): F[Option[Period]] = {
    indexClient.computePeriodIndexesFrom(statements).map { indexes =>
      indexes.lastOption.map { periodIndex =>
        Period(periodIndex)
      }
    }
  }
}

object AnalyticsClient {
  def apply[F[_]](
    icomptaClient: IComptaClient[F],
    dbClient: DBClient[F],
    settings: Settings
  )(implicit F: Effect[F]): AnalyticsClient[F] = {
    val indexClient = AnalyticsIndexClient(icomptaClient, settings)
    AnalyticsClient(indexClient, icomptaClient, dbClient, settings)
  }
}
