package sre.api
package finance
package analytics

import cats.data.OptionT
import icompta.IComptaClient
import cm.{ CMClient, CMAccount }
import cats.effect._
import cats.implicits._

case class AnalyticsClient[F[_]](
  analyticsIndex: AnalyticsIndex[F],
  icomptaClient: IComptaClient[F],
  dbClient: DBClient[F],
  cmClient: CMClient[F],
  settings: Settings
)(implicit F: Effect[F]) {

  def reindex(): F[Unit] = {
    analyticsIndex.buildHistoryIndexes().flatMap { indexes =>
      dbClient.upsertPeriodIndexes(indexes)
    }
  }

  def computeExpensesByCategory(account: CMAccount): OptionT[F, List[ExpensesByCategory]] = {
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

  def getPreviousPeriods(): F[List[Period]] = {
    dbClient.selectAllPeriodIndexes().map { periodIndexes =>
      periodIndexes.map { periodIndex =>
        Period(periodIndex.startDate, periodIndex.endDate, periodIndex.balance)
      }
    }
  }

  def computeCurrentPeriod(accounts: List[CMAccount]): F[Option[Period]] = {
    val allStatements = accounts.flatMap(_.statements)

    analyticsIndex.buildIndexes(allStatements).map { indexes =>
      indexes.lastOption.map { periodIndex =>
        Period(periodIndex.startDate, periodIndex.endDate, periodIndex.balance)
      }
    }
  }
}

object AnalyticsClient {
  def apply[F[_]](
    icomptaClient: IComptaClient[F],
    dbClient: DBClient[F],
    cmClient: CMClient[F],
    settings: Settings
  )(implicit F: Effect[F]): AnalyticsClient[F] = {
    val analyticsIndex = AnalyticsIndex(icomptaClient, dbClient, settings)
    AnalyticsClient(analyticsIndex, icomptaClient, dbClient, cmClient, settings)
  }
}
