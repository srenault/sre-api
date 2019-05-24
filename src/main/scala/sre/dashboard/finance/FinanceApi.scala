package sre.dashboard.finance

import cats.effect._
import cats.data.OptionT
import cats.implicits._
import sre.dashboard.Settings

case class FinanceApi[F[_]](icomptaClient: IComptaClient[F], cmClient: CMClient[F], settings: Settings)(implicit F: Effect[F]) {

  private def withRecordsAst[A](f: RuleAst => A): F[A] = {
    icomptaClient.selectAll().map { records =>
      val rulesAst = RulesAst.build(records)
      f(rulesAst)
    }
  }

  def computeExpensesByCategory(account: CMAccount): OptionT[F, List[ExpensesByCategory]] = {
    OptionT.liftF(withRecordsAst { rulesAst =>
      val categories = settings.finance.cm.accounts
        .find(_.id == account.id)
        .map(_.categories)
        .getOrElse(Map.empty)

      categories.flatMap {
        case (categoryId, category) =>
          rulesAst.traverse(category.path) map { ruleAst =>
            val amount = account.statements.foldLeft(0F) { (acc, statement) =>
              if (ruleAst.test(statement)) {
                acc + scala.math.abs(statement.amount)
              } else {
                acc
              }
            }
            ExpensesByCategory(categoryId, category.label, amount, category.threshold)
          }
      }.toList
    })
  }

  def filterStatementsForPeriod(accounts: List[CMAccount]): F[List[CMStatement]] = {
    val categoryLabel = settings.finance.icompta.wageCategory
    icomptaClient.selectScheduledTransactionsByCategoryLabel(categoryLabel).map { wageTransactions =>
      val statements = accounts.flatMap(_.statements)
      val maybeWageStatement = statements.sortBy(-_.date.toEpochDay).filter { statement =>
        wageTransactions.exists(wageTransaction => statement.label.startsWith(wageTransaction.name))
      }.take(2).lastOption
      maybeWageStatement match {
        case Some(wageStatement) =>
          statements.sortBy(-_.date.toEpochDay).takeWhile { statement =>
            statement.date.equals(wageStatement.date) || statement.date.isAfter(wageStatement.date)
          }.reverse

        case _ => Nil
      }
    }
  }

  def f() = {

  }
}
