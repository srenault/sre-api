package sre.api.finance

import cats.effect._
import cats.implicits._
import org.http4s._
import org.http4s.circe._
import io.circe.literal._
import io.circe.syntax._
import cm.CMClient
import analytics.AnalyticsClient

case class FinanceService[F[_]](
  cmClient: CMClient[F],
  dbClient: DBClient[F],
  settings: Settings
)(implicit F: Sync[F]) extends FinanceServiceDsl[F] {

  lazy val analyticsClient = AnalyticsClient(dbClient, settings)

  val routes: HttpRoutes[F] =
    HttpRoutes.of[F] {

      case GET -> Root / "accounts" =>
        WithAccountsOverview() { accountsOverview =>
          Ok(accountsOverview.asJson)
        }

      case GET -> Root / "analytics" :? OptionalBeforePeriodDateQueryParamMatcher(maybeValidatedBeforePeriod) +& OptionalAfterPeriodDateQueryParamMatcher(maybeValidatedAfterPeriod) =>
        WithPeriodDate(maybeValidatedBeforePeriod) { maybeBeforePeriod =>
          WithPeriodDate(maybeValidatedAfterPeriod) { maybeAfterPeriod =>
            for {
              periods <- analyticsClient.getPeriods(maybeBeforePeriod, maybeAfterPeriod, limit = 10)

              lastPeriod = periods.lastOption.flatMap(_.yearMonth)

              headPeriod = periods.headOption.flatMap(_.yearMonth)

              (hasPreviousPage, hasNextPage) <- {
                (maybeBeforePeriod -> maybeAfterPeriod) match {
                  case (Some(_), Some(_)) =>
                    F.pure(false -> false)

                  case (Some(beforePeriod), None) =>
                    analyticsClient.countPeriods(maybeBeforePeriod = None, maybeAfterPeriod = Some(beforePeriod)).flatMap { countPrevious =>
                      analyticsClient.countPeriods(maybeBeforePeriod = lastPeriod, maybeAfterPeriod = None).map { countNext =>
                        (countPrevious > 0) -> (countNext > 0)
                      }
                    }

                  case (None, Some(afterPeriod)) =>
                    analyticsClient.countPeriods(maybeBeforePeriod = Some(afterPeriod), maybeAfterPeriod = None).flatMap { countPrevious =>
                      analyticsClient.countPeriods(maybeBeforePeriod = None, maybeAfterPeriod = headPeriod).map { countNext =>
                        (countPrevious > 0) -> (countNext > 0)
                      }
                    }

                  case (None, None) =>
                    analyticsClient.countPeriods(maybeBeforePeriod = None, maybeAfterPeriod = headPeriod).flatMap { countPrevious =>
                      analyticsClient.countPeriods(maybeBeforePeriod = lastPeriod, maybeAfterPeriod = None).map { countNext =>
                        (countPrevious > 0) -> (countNext > 0)
                      }
                    }
                }
              }

              result <- Ok(json"""{ "result": $periods, "hasPreviousPage": $hasPreviousPage, "hasNextPage": $hasNextPage }""")
            } yield result
          }
        }

      case GET -> Root / "analytics" / "period" / PeriodDateVar(periodDate) =>
        analyticsClient.getStatementsForPeriod(periodDate).value.flatMap {
          case Some((period, statements)) =>
            Ok(json"""{ "statements": $statements, "period":  $period }""")

          case None =>
            NotFound()
        }
      }
}
