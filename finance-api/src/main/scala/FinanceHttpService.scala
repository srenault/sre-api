package sre.api
package finance

import cats.effect._
import cats.implicits._
import org.http4s._
import org.http4s.circe._
import io.circe._
import io.circe.literal._
import io.circe.syntax._
import cm._
import org.typelevel.log4cats.Logger
import analytics.{ Period, AnalyticsClient }
import settings.FinanceSettings

case class FinanceHttpService[F[_] : Logger](
  cmClient: CMClient[F],
  dbClient: DBClient[F],
  settings: FinanceSettings
)(implicit F: Sync[F]) extends FinanceHttpServiceDsl[F] {

  lazy val analyticsClient = AnalyticsClient(dbClient, settings)

  val service = new FinanceService(cmClient, dbClient, settings)

  val routes: HttpRoutes[F] =
    HttpRoutes.of[F] {

      case GET -> Root / "finance" / "accounts" =>
        service.getAccountsOverview().flatMap {
          case Some(accountsOverview) =>
            val json = FinanceHttpService.accountsOverviewEncoder(accountsOverview)
            Ok(json)
          case None =>
            NotFound()
        }

      case GET -> Root / "accounts" / AccountIdVar(accountId) :? OptionalPeriodDateQueryParamMatcher(maybeValidatedPeriod) =>
        WithPeriodDate(maybeValidatedPeriod) { maybePeriodDate =>
          service.getAccountState(accountId, maybePeriodDate).flatMap {
            case Some(periodWithAccountState) =>
              val json = FinanceHttpService.accountStateEncoder(periodWithAccountState)
              Ok(json)

            case None =>
              NotFound()
          }
        }

      case GET -> Root / "finance" / "analytics" :? OptionalBeforePeriodDateQueryParamMatcher(maybeValidatedBeforePeriod) +& OptionalAfterPeriodDateQueryParamMatcher(maybeValidatedAfterPeriod) =>
        WithPeriodDate(maybeValidatedBeforePeriod) { maybeBeforePeriod =>
          WithPeriodDate(maybeValidatedAfterPeriod) { maybeAfterPeriod =>
            service.getPeriods(maybeBeforePeriod, maybeAfterPeriod).flatMap { periodsWithPagination =>
              val (periods, hasPreviousPage, hasNextPage) = periodsWithPagination
              Ok(json"""{ "result": $periods, "hasPreviousPage": $hasPreviousPage, "hasNextPage": $hasNextPage }""")
            }
          }
        }

      case GET -> Root / "finance" / "analytics" / "period" / PeriodDateVar(periodDate) =>
        service.getStatementsForPeriod(periodDate).flatMap {
          case Some(periodWithStatements) =>
            Ok(FinanceHttpService.statementsEncoder(periodWithStatements))

          case None =>
            NotFound()
        }
      }
}

object FinanceHttpService {

  val accountsOverviewEncoder: Encoder[CMAccountsOverview] = new Encoder[CMAccountsOverview] {
    final def apply(accountsOverview: CMAccountsOverview): Json = {
      accountsOverview.asJson(CMAccountsOverview.encoder)
    }
  }

  val accountStateEncoder: Encoder[(Period, CMAccountState)] = new Encoder[(Period, CMAccountState)] {
    final def apply(periodWithAccountState: (Period, CMAccountState)): Json = {
      val (period, accountState) = periodWithAccountState
      json"""{ "period": $period , "account": $accountState }"""
    }
  }

  val periodsEncoder: Encoder[(List[Period], Boolean, Boolean)] = new Encoder[(List[Period], Boolean, Boolean)] {
    final def apply(periodsWithPagination: (List[Period], Boolean, Boolean)): Json = {
      val (periods, hasPreviousPage, hasNextPage) = periodsWithPagination
      json"""{ "result": $periods, "hasPreviousPage": $hasPreviousPage, "hasNextPage": $hasNextPage }"""
    }
  }

  val statementsEncoder: Encoder[(Period, List[CMStatement])] = new Encoder[(Period, List[CMStatement])] {
    final def apply(periodWithStatements: (Period, List[CMStatement])): Json = {
      val (period, statements) = periodWithStatements
      json"""{ "statements": $statements, "period":  $period }"""
    }
  }
}
