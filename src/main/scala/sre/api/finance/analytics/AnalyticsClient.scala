package sre.api
package finance
package analytics

import java.time.LocalDate
import cats.data.OptionT
import cats.effect._
import cats.implicits._
import cm.{ CMAccountState, CMStatement }
import icompta.IComptaClient

case class AnalyticsClient[F[_]](
  analyticsIndex: AnalyticsIndex[F],
  icomptaClient: IComptaClient[F],
  dbClient: DBClient[F],
  settings: Settings
)(implicit F: Effect[F]) {

  def reindex(): F[Unit] = {
    analyticsIndex.buildHistoryIndexes().flatMap { indexes =>
      dbClient.upsertPeriodIndexes(indexes)
    }
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

  def getAccountStateAt(accountId: String, startPeriod: LocalDate): OptionT[F, CMAccountState] = {
    for {
      accountSettings <- OptionT(F.pure(settings.finance.cm.accounts.find(_.id == accountId)))

      periodIndex <- OptionT(dbClient.selectOnePeriodIndex(startPeriod))

      statements <- OptionT.liftF {
        periodIndex.partitions
          .filter(_.accountId == accountId)
          .map(f => finance.ofx.OfxStmTrn.load(f))
          .sequence.map { ofxStmTrn =>
            ofxStmTrn
              .flatten
              .map(_.toStatement(accountId))
              .sortBy(_.date.toEpochDay)
              .dropWhile(_.date.isBefore(periodIndex.startDate))
              .takeWhile(st => periodIndex.endDate.map(endDate => st.date.isBefore(endDate.plusDays(1))) getOrElse true)
          }
      }
    } yield {
      CMAccountState(accountSettings, statements)
    }
  }

  def getPreviousPeriods(): F[List[Period]] = {
    dbClient.selectAllPeriodIndexes().map { periodIndexes =>
      periodIndexes.map { periodIndex =>
        Period(periodIndex.startDate, periodIndex.endDate, periodIndex.balance)
      }
    }
  }

  def computeCurrentPeriod(statements: List[CMStatement]): F[Option[Period]] = {
    analyticsIndex.buildIndexes(statements).map { indexes =>
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
    settings: Settings
  )(implicit F: Effect[F]): AnalyticsClient[F] = {
    val analyticsIndex = AnalyticsIndex(icomptaClient, dbClient, settings)
    AnalyticsClient(analyticsIndex, icomptaClient, dbClient, settings)
  }
}
