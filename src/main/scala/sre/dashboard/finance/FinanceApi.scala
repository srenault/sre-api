package sre.dashboard.finance

import java.time.LocalDate
import cats.effect._
import cats.data.OptionT
import cats.implicits._
import sre.dashboard.Settings

case class FinanceApi[F[_]: Effect](icomptaClient: IComptaClient[F], settings: Settings) {

  def computeExpensesByCategory(transactions: List[OfxStmTrn], date: LocalDate): OptionT[F, Map[String, Option[Float]]] = {
    OptionT.liftF(icomptaClient.selectAll().map { records =>
      val rulesAst = RulesAst.build(records)
      settings.finance.icompta.categories.mapValues { path =>
        rulesAst.traverse(path) map { ruleAst =>
          transactions.foldLeft(0F) { (acc, transaction) =>
            if (ruleAst.test(transaction)) {
              acc + scala.math.abs(transaction.amount)
            } else {
              acc
            }
          }
        }
      }
    })
  }
}
