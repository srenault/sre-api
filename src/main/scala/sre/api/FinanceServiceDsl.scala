package sre.api

import java.time.YearMonth
import java.time.format.DateTimeFormatterBuilder

import cats._
import cats.effect._
import cats.implicits._
import cats.data.{EitherT, Validated, ValidatedNel}
import cats.data.Validated.{Invalid, Valid}
import io.circe.literal._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import finance.cm.{CMAccountState, CMAccountsOverview, CMClient, CMOtpRequest}
import finance.analytics.AnalyticsClient
import finance.icompta.IComptaClient
import sre.api.finance.models.Period

trait FinanceServiceDsl[F[_]] extends Http4sDsl[F] {

  def icomptaClient: IComptaClient[F]

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
    def decode(value: QueryParameterValue): ValidatedNel[ParseFailure, YearMonth] =
      validatePeriodDate(value.value)
  }

  object ReindexFromScrachQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Boolean]("fromScratch")

  object OptionalPeriodDateQueryParamMatcher extends OptionalValidatingQueryParamDecoderMatcher[YearMonth]("periodDate")

  object PeriodDateVar {
    def unapply(str: String): Option[YearMonth] = validatePeriodDate(str).toOption
  }

  object AccountIdVar {
    def unapply(str: String): Option[String] = {
      if (!str.isEmpty) Some(str) else None
    }
  }

  def WithPeriodDate(maybeValidatedDate: Option[ValidatedNel[ParseFailure, YearMonth]])(f: Option[YearMonth] => F[Response[F]])(implicit F: Monad[F]): F[Response[F]] = {
    maybeValidatedDate match {
      case Some(Invalid(e)) => BadRequest(s"Invalid date: $e")
      case Some(Valid(date)) => f(Some(date))
      case None => f(None)
    }
  }

  protected def handleOtpRequest[A](otpOrResult: EitherT[F, CMOtpRequest, A])(f: A => F[Response[F]])(implicit F: Effect[F]): F[Response[F]] = {
    otpOrResult.value.flatMap {
      case Left(otpRequest) =>
        Ok(json"""{ "otpRequired": $otpRequest }""")

      case Right(result) =>
        f(result)
    }
  }

  def WithAccountsOverview()(f: CMAccountsOverview => F[Response[F]])(implicit F: Effect[F]): F[Response[F]] =
    handleOtpRequest(cmClient.fetchAccountsState()) { accountsState =>
      val allStatements = accountsState.flatMap(_.statements)
      analyticsClient.computeCurrentPeriod(allStatements).flatMap {
        case Some(period) =>
          val accountsOverview = CMAccountsOverview(period, accountsState.map(_.toOverview))
          f(accountsOverview)

        case None =>
          NotFound()
      }
    }

  def WithAccountState(accountId: String, maybePeriodDate: Option[YearMonth])(f: (Period, CMAccountState) => F[Response[F]])(implicit F: Effect[F]): F[Response[F]] = {
    maybePeriodDate match {
      case Some(periodDate) =>
        analyticsClient.getAccountStateForPeriod(accountId, periodDate).value.flatMap {
          case Some((period, accountState)) =>
            f(period, accountState)

          case None =>
            NotFound()
        }

      case None =>
        handleOtpRequest(cmClient.fetchAccountsState()) { accountsState =>
          val allStatements = accountsState.flatMap(_.statements)
          analyticsClient.computeCurrentPeriod(allStatements).flatMap {
            case Some(period) =>
              accountsState.find(_.id === accountId) match {
                case Some(accountState) => f(period, accountState.forPeriod(period))
                case None => NotFound()
              }

            case None =>
              NotFound()
          }
        }
    }
  }
}
