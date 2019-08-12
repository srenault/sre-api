package sre.api

import cats.effect._
import cats.implicits._
import org.http4s._
import org.http4s.circe._
import io.circe.literal._
import io.circe.syntax._
import finance.icompta.IComptaClient
import finance.cm.CMClient
import finance.analytics.{ AnalyticsClient, Period }

case class FinanceService[F[_]: Effect](
  icomptaClient: IComptaClient[F],
  cmClient: CMClient[F],
  dbClient: DBClient[F],
  settings: Settings
) extends FinanceServiceDsl[F] {

  val analyticsClient = AnalyticsClient(icomptaClient, dbClient, cmClient, settings)

  val service: HttpService[F] = CorsMiddleware {
    HttpService[F] {

      case GET -> Root / "accounts" =>

        for {

          accounts <- cmClient.fetchAccounts()

          maybePeriod: Option[Period] <- analyticsClient.computeCurrentPeriod(accounts)

          res <- maybePeriod match {
            case Some(period) =>
              Ok(json"""{ "period": $period, "accounts": $accounts }""")

            case None =>
              NotFound()
          }

        } yield res

      case GET -> Root / "accounts" / AccountIdVar(accountId) :? DateQueryParamMatcher(maybeValidatedDate) =>
        WithAccount(accountId) { account =>
          WithPeriodDate(maybeValidatedDate) { maybeStartPeriod =>
            val accountSinceStartPeriod = maybeStartPeriod map(account.since) getOrElse account
            analyticsClient.computeExpensesByCategory(accountSinceStartPeriod).value.flatMap { expenses =>
              val json = accountSinceStartPeriod.asJson.deepMerge(json"""{ "expenses": $expenses }""")
              Ok(json)
            }
          }
        }

      case GET -> Root / "analytics" =>
        analyticsClient.getPreviousPeriods().flatMap { periods =>
          Ok(json"""{ "result": $periods }""")
        }

      case GET -> Root / "analytics" / "reindex" =>
        analyticsClient.reindex().flatMap(_ => Ok())
    }
  }
}
