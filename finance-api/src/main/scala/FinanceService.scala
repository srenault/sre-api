package sre.api
package finance

import java.time.YearMonth
import cats.effect._
import cats.implicits._
import org.http4s._
import org.http4s.circe._
import io.circe.literal._
import io.circe.syntax._
import org.typelevel.log4cats.Logger
import analytics.Period
import cm._
import analytics.AnalyticsClient
import settings.FinanceSettings

case class FinanceService[F[_]: Logger](
    cmClient: CMClient[F],
    dbClient: DBClient[F],
    settings: FinanceSettings
)(implicit F: Sync[F]) {

  lazy val analyticsClient = AnalyticsClient(dbClient, settings)

  def getAccountsOverview(): F[Option[CMAccountsOverview]] = {
    dbClient.selectLastPeriod().flatMap {
      case Some(lastPeriod) =>
        dbClient
          .selectStatements(maybeAfterDate = Some(lastPeriod.endDate))
          .flatMap { allStatements =>
            analyticsClient.computeCurrentPeriod(allStatements) match {
              case Some(period) =>
                val accounts =
                  allStatements.groupBy(_.accountId).toList.flatMap {
                    case (accountId, statements) =>
                      settings.cm.accounts.find(_.id == accountId).map {
                        accountSettings =>
                          CMAccountState(
                            id = accountId,
                            `type` = accountSettings.`type`,
                            label = Some(accountSettings.label),
                            displayName = Some(accountSettings.label),
                            statements = statements
                          ).toOverview
                      }
                  }
                F.pure(Some(CMAccountsOverview(period, accounts)))

              case None =>
                Logger[F]
                  .warn(s"No period were found from statements")
                  .map(_ => None)
            }
          }
      case None =>
        Logger[F].warn(s"No last period has been found").map(_ => None)
    }
  }

  def getAccountState(
      accountId: String,
      maybePeriodDate: Option[YearMonth]
  ): F[Option[(Period, CMAccountState)]] = {
    val maybeAccountSettings = settings.cm.accounts.find(_.id == accountId)

    (maybeAccountSettings, maybePeriodDate) match {
      case (None, _) =>
        Logger[F].warn(s"Unknown acccount $accountId").map(_ => None)

      case (_, Some(periodDate)) =>
        analyticsClient
          .getAccountStateForPeriod(accountId, periodDate)
          .value
          .flatMap {
            case Some((period, accountState)) =>
              F.pure(Some(period -> accountState))

            case None =>
              Logger[F]
                .warn(s"Not able to get account state for period ${periodDate}")
                .map(_ => None)
          }

      case (Some(accountSettings), None) =>
        dbClient.selectLastPeriod().flatMap {
          case Some(lastPeriod) =>
            dbClient
              .selectStatementsByAccountId(
                accountId,
                maybeAfterDate = Some(lastPeriod.endDate)
              )
              .flatMap { statements =>
                analyticsClient.computeCurrentPeriod(statements) match {
                  case Some(period) =>
                    val accountState = CMAccountState(
                      id = accountId,
                      `type` = accountSettings.`type`,
                      label = Some(accountSettings.label),
                      displayName = Some(accountSettings.label),
                      statements = statements
                    )

                    F.pure(Some(period -> accountState.forPeriod(period)))

                  case None =>
                    Logger[F]
                      .warn("No period were found from statements")
                      .map(_ => None)
                }
              }

          case None =>
            Logger[F].warn("No last period has been found").map(_ => None)
        }
    }
  }

  def getPeriods(
      maybeBeforePeriod: Option[YearMonth],
      maybeAfterPeriod: Option[YearMonth]
  ): F[(List[Period], Boolean, Boolean)] = {
    for {
      periods <- analyticsClient.getPeriods(
        maybeBeforePeriod,
        maybeAfterPeriod,
        limit = 10
      )

      lastPeriod = periods.lastOption.flatMap(_.yearMonth)

      headPeriod = periods.headOption.flatMap(_.yearMonth)

      (hasPreviousPage, hasNextPage) <- {
        (maybeBeforePeriod -> maybeAfterPeriod) match {
          case (Some(_), Some(_)) =>
            F.pure(false -> false)

          case (Some(beforePeriod), None) =>
            analyticsClient
              .countPeriods(
                maybeBeforePeriod = None,
                maybeAfterPeriod = Some(beforePeriod)
              )
              .flatMap { countPrevious =>
                analyticsClient
                  .countPeriods(
                    maybeBeforePeriod = lastPeriod,
                    maybeAfterPeriod = None
                  )
                  .map { countNext =>
                    (countPrevious > 0) -> (countNext > 0)
                  }
              }

          case (None, Some(afterPeriod)) =>
            analyticsClient
              .countPeriods(
                maybeBeforePeriod = Some(afterPeriod),
                maybeAfterPeriod = None
              )
              .flatMap { countPrevious =>
                analyticsClient
                  .countPeriods(
                    maybeBeforePeriod = None,
                    maybeAfterPeriod = headPeriod
                  )
                  .map { countNext =>
                    (countPrevious > 0) -> (countNext > 0)
                  }
              }

          case (None, None) =>
            analyticsClient
              .countPeriods(
                maybeBeforePeriod = None,
                maybeAfterPeriod = headPeriod
              )
              .flatMap { countPrevious =>
                analyticsClient
                  .countPeriods(
                    maybeBeforePeriod = lastPeriod,
                    maybeAfterPeriod = None
                  )
                  .map { countNext =>
                    (countPrevious > 0) -> (countNext > 0)
                  }
              }
        }
      }
    } yield (periods, hasPreviousPage, hasNextPage)
  }

  def getStatementsForPeriod(
      periodDate: YearMonth
  ): F[Option[(Period, List[CMStatement])]] = {
    analyticsClient.getStatementsForPeriod(periodDate).value
  }
}
