package sre.api

import cats.effect._
import cats.implicits._
import org.http4s._
import org.http4s.circe._
import io.circe.literal._
import io.circe.syntax._
import finance.icompta.IComptaClient
import finance.cm.CMClient
import finance.analytics.AnalyticsClient

case class FinanceService[F[_]: ConcurrentEffect : Timer](
  icomptaClient: IComptaClient[F],
  cmClient: CMClient[F],
  dbClient: DBClient[F],
  settings: Settings
) extends FinanceServiceDsl[F] {

  lazy val analyticsClient = AnalyticsClient(icomptaClient, dbClient, settings)

  val service: HttpService[F] = CorsMiddleware {
    HttpService[F] {

      case GET -> Root / "otp" / transactionId / "status" =>
        cmClient.checkOtpStatus(transactionId).flatMap { otpStatus =>
          Ok(otpStatus)
        }

      case GET -> Root / "accounts" =>
        WithAccountsOverview() { accountsOverview =>
          Ok(accountsOverview.asJson)
        }

      case GET -> Root / "accounts" / AccountIdVar(accountId) :? PeriodQueryParamMatcher(validatedPeriod) =>
        WithPeriodDate(validatedPeriod) { periodDate =>
          WithAccountState(accountId, periodDate) { accountState =>
            analyticsClient.computeExpensesByCategory(accountState).value.flatMap { expenses =>
              val json = accountState.asJson.deepMerge(json"""{ "expenses": $expenses }""")
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
