package sre.dashboard

import cats.effect._
import cats.implicits._
import org.http4s._
import org.http4s.circe._
import io.circe.literal._
import io.circe.syntax._
import finance._

case class FinanceService[F[_]: Effect](icomptaClient: IComptaClient[F], cmClient: CMClient[F], settings: Settings) extends FinanceServiceDsl[F] {

  val financeApi = FinanceApi(icomptaClient, cmClient, settings)

  val service: HttpService[F] = CorsMiddleware {
    HttpService[F] {

      case GET -> Root / "accounts" =>

        for {
          accounts <- cmClient.fetchAccounts()

          statements <- financeApi.filterStatementsForPeriod(accounts)

          (credit, debit) = CMStatement.computeCreditAndDebit(statements)

          startPeriod = statements.headOption.map(_.date)

          res <- Ok(json"""{ "startPeriod": $startPeriod,
                             "credit": $credit,
                             "debit": $debit,
                             "accounts": $accounts }""")
        } yield res

      case GET -> Root / "accounts" / AccountIdVar(accountId) :? DateQueryParamMatcher(maybeValidatedDate) =>
        WithAccount(accountId) { account =>
          WithPeriodDate(maybeValidatedDate) { maybeStartPeriod =>
            val accountSinceStartPeriod = maybeStartPeriod map(account.since) getOrElse account
            financeApi.computeExpensesByCategory(accountSinceStartPeriod).value.flatMap { expenses =>
              val json = accountSinceStartPeriod.asJson.deepMerge(json"""{ "expenses": $expenses }""")
              Ok(json)
            }
          }
        }
    }
  }
}
