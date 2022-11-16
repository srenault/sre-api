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

  def service: FinanceService[F]

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
}
