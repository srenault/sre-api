package sre.api
package finance

import java.time.YearMonth
import java.time.format.DateTimeFormatterBuilder
import cats._
import cats.effect._
import cats.implicits._
import cats.data.{Validated, ValidatedNel, EitherT}
import cats.data.Validated.{Invalid, Valid}
import io.circe.literal._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import cm.{CMClient, CMAccountState, CMAccountsOverview, CMOtpRequest}
import analytics.{AnalyticsClient, Period}
import settings.FinanceSettings

trait FinanceHttpServiceDsl[F[_]] extends Http4sDsl[F] {

  def settings: FinanceSettings

  def cmClient: CMClient[F]

  def dbClient: DBClient[F]

  def analyticsClient: AnalyticsClient[F]

  def validatePeriodDate(str: String): ValidatedNel[ParseFailure, YearMonth] = {
    Validated
      .catchNonFatal {
        val format = new DateTimeFormatterBuilder()
          .appendPattern("yyyy-MM")
          .toFormatter();
        val date = YearMonth.parse(str, format)
        YearMonth.from(date)
      }
      .leftMap(t => ParseFailure(s"Query decoding period failed", t.getMessage))
      .toValidatedNel
  }

  implicit val periodQueryParamDecoder = new QueryParamDecoder[YearMonth] {
    def decode(
        value: QueryParameterValue
    ): ValidatedNel[ParseFailure, YearMonth] =
      validatePeriodDate(value.value)
  }

  object ReindexFromScrachQueryParamMatcher
      extends OptionalQueryParamDecoderMatcher[Boolean]("fromScratch")

  object OptionalPeriodDateQueryParamMatcher
      extends OptionalValidatingQueryParamDecoderMatcher[YearMonth](
        "periodDate"
      )

  object OptionalBeforePeriodDateQueryParamMatcher
      extends OptionalValidatingQueryParamDecoderMatcher[YearMonth]("before")

  object OptionalAfterPeriodDateQueryParamMatcher
      extends OptionalValidatingQueryParamDecoderMatcher[YearMonth]("after")

  object PeriodDateVar {
    def unapply(str: String): Option[YearMonth] = validatePeriodDate(
      str
    ).toOption
  }

  object AccountIdVar {
    def unapply(str: String): Option[String] = {
      if (!str.isEmpty) Some(str) else None
    }
  }

  def WithPeriodDate(
      maybeValidatedDate: Option[ValidatedNel[ParseFailure, YearMonth]]
  )(
      f: Option[YearMonth] => F[Response[F]]
  )(implicit F: Monad[F]): F[Response[F]] = {
    maybeValidatedDate match {
      case Some(Invalid(e))  => BadRequest(s"Invalid date: $e")
      case Some(Valid(date)) => f(Some(date))
      case None              => f(None)
    }
  }

  def WithAccountsOverview()(
      f: CMAccountsOverview => F[Response[F]]
  )(implicit F: Sync[F]): F[Response[F]] =
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
                f(CMAccountsOverview(period, accounts))

              case None =>
                NotFound("No period were found from statements")
            }
          }
      case None =>
        NotFound("No last period has been found")
    }

  def WithAccountState(accountId: String, maybePeriodDate: Option[YearMonth])(
      f: (Period, CMAccountState) => F[Response[F]]
  )(implicit F: Sync[F]): F[Response[F]] = {
    val maybeAccountSettings = settings.cm.accounts.find(_.id == accountId)

    (maybeAccountSettings, maybePeriodDate) match {
      case (None, _) =>
        NotFound()

      case (_, Some(periodDate)) =>
        analyticsClient
          .getAccountStateForPeriod(accountId, periodDate)
          .value
          .flatMap {
            case Some((period, accountState)) =>
              f(period, accountState)

            case None =>
              NotFound(
                s"Not able to get account state for period ${periodDate}"
              )
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

                    f(period, accountState.forPeriod(period))

                  case None =>
                    NotFound("No period were found from statements")
                }
              }

          case None =>
            NotFound("No last period has been found")
        }
    }
  }
}
