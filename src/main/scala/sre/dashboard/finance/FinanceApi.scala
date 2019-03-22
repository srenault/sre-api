package sre.dashboard.finance

import java.time.LocalDate
import cats.effect._
import cats.data.OptionT
import cats.implicits._
import sre.dashboard.Settings

case class FinanceApi[F[_]: Effect](icomptaClient: IComptaClient[F], settings: Settings) {

  def computeExpensesByCategory(transactions: List[OfxStmTrn], date: LocalDate): OptionT[F, List[ExpensesByCategory]] = {
    OptionT.liftF(icomptaClient.selectAll().map { records =>
      val rulesAst = RulesAst.build(records)
      settings.finance.icompta.categories.flatMap {
        case (categoryId, category) =>
          rulesAst.traverse(category.path) map { ruleAst =>
            val amount = transactions.foldLeft(0F) { (acc, transaction) =>
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

  def computeCreditAndDebit(transactions: List[OfxStmTrn]): (Float, Float) = {
    val (credits, debits) = transactions.map(_.amount).partition(_ > 0)
    credits.reduce(_ + _) -> debits.reduce(_ + _)
  }
}
