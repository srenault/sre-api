package sre.api

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

          maybePeriod <- financeApi.computeLastPeriod(accounts.flatMap(_.statements))

          statementsForPeriod = maybePeriod.toList.flatMap(_.statements)

          (credit, debit) = CMStatement.computeCreditAndDebit(statementsForPeriod)

          res <- Ok(json"""{ "startPeriod": ${maybePeriod.map(_.startDate)},
                             "credit": $credit,
                             "debit": $debit,
                             "accounts": $accounts }""")
        } yield res // TODO NOT FOUND

      case GET -> Root / "accounts" / AccountIdVar(accountId) :? DateQueryParamMatcher(maybeValidatedDate) =>
        org.slf4j.LoggerFactory.getLogger("sre.api.FinanceService.account").info("-----------> " + maybeValidatedDate)
        WithAccount(accountId) { account =>
          WithPeriodDate(maybeValidatedDate) { maybeStartPeriod =>
            val accountSinceStartPeriod = maybeStartPeriod map(account.since) getOrElse account
            financeApi.computeExpensesByCategory(accountSinceStartPeriod).value.flatMap { expenses =>
              val json = accountSinceStartPeriod.asJson.deepMerge(json"""{ "expenses": $expenses }""")
              Ok(json)
            }
          }
        }

      case GET -> Root / "test" =>
        financeApi.f().flatMap(_ => Ok())
    }
  }
}
