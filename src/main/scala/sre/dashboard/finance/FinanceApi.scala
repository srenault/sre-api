package sre.dashboard.finance

import java.time.LocalDate
import cats.effect._
import cats.data.OptionT
import cats.implicits._
import sre.dashboard.Settings

case class FinanceApi[F[_]](icomptaClient: IComptaClient[F], cmClient: CMClient[F], settings: Settings)(implicit F: Effect[F]) {

  def withRecordsAst[A](f: RuleAst => A): F[A] = {
    icomptaClient.selectAll().map { records =>
      val rulesAst = RulesAst.build(records)
      f(rulesAst)
    }
  }

  def computeExpensesByCategory(accountId: String, statements: List[CMStatement], date: LocalDate): OptionT[F, List[ExpensesByCategory]] = {
    OptionT.liftF(withRecordsAst { rulesAst =>
      val categories = settings.finance.cm.accounts
        .find(_.id == accountId)
        .map(_.categories)
        .getOrElse(Map.empty)

      categories.flatMap {
        case (categoryId, category) =>
          rulesAst.traverse(category.path) map { ruleAst =>
            val amount = statements.foldLeft(0F) { (acc, transaction) =>
              if (ruleAst.test(transaction)) {
                acc + scala.math.abs(transaction.amount)
              } else {
                acc
              }
            }
            ExpensesByCategory(categoryId, category.label, amount, category.threshold)
          }
      }.toList
    })
  }

  def fetchStatementsForPeriod(accounts: List[CMAccount], date: LocalDate): F[List[CMStatement]] = {
    val categoryLabel = settings.finance.icompta.wageCategory
    val startDate = date.minusMonths(1).withDayOfMonth(15)

    icomptaClient.selectScheduledTransactionsByCategoryLabel(categoryLabel).flatMap[List[CMStatement]] { scheduledTransactions =>
      val sortedScheduledTransactions = scheduledTransactions.sortBy(_.date.toEpochDay)
      sortedScheduledTransactions.headOption match {
        case None =>
          F.pure(Nil)

        case Some(wageTransaction) =>
          accounts.map { account =>
            cmClient.fetchStatements(account.id, maybeStartDate = Some(startDate))
          }.sequence.map(_.flatten).map { statements =>
            val sortedStatements = statements.sortBy(-_.date.toEpochDay)
            sortedStatements.find(_.label.startsWith(wageTransaction.name)) match {
              case Some(wageStatement) =>
                sortedStatements.takeWhile { statement =>
                  statement.date.equals(wageStatement.date) || statement.date.isAfter(wageStatement.date)
                }.reverse

              case None => Nil

            }
          }
      }
    }
  }

  def computeCreditAndDebit(transactions: List[CMStatement]): (Float, Float) = {
    val (credits, debits) = transactions.map(_.amount).partition(_ > 0)
    credits.foldLeft(0F)(_ + _) -> debits.foldLeft(0F)(_ + _)
  }
}
