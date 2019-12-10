package sre.api

import java.time.YearMonth
import java.time.format.DateTimeFormatterBuilder
import cats._
import cats.effect._
import cats.implicits._
import cats.data.{ Validated, ValidatedNel }
import cats.data.Validated.{ Invalid, Valid }
import io.circe.literal._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import finance.cm.{ CMClient, CMAccountState, CMAccountsOverview }
import finance.analytics.AnalyticsClient
import finance.icompta.IComptaClient

trait FinanceServiceDsl[F[_]] extends Http4sDsl[F] {

  def icomptaClient: IComptaClient[F]

  def cmClient: CMClient[F]

  def dbClient: DBClient[F]

  def analyticsClient: AnalyticsClient[F]

  implicit val periodQueryParamDecoder = new QueryParamDecoder[YearMonth] {
    def decode(value: QueryParameterValue): ValidatedNel[ParseFailure, YearMonth] =
      Validated
        .catchNonFatal {
          val format = new DateTimeFormatterBuilder()
            .appendPattern("yyyy-MM")
            .toFormatter();
          val date = YearMonth.parse(value.value, format)
          YearMonth.from(date)
        }
        .leftMap(t => ParseFailure(s"Query decoding period failed", t.getMessage))
        .toValidatedNel
  }

  object PeriodQueryParamMatcher extends OptionalValidatingQueryParamDecoderMatcher[YearMonth]("period")

  object AccountIdVar {
    def unapply(str: String): Option[String] = {
      if (!str.isEmpty) Some(str) else None
    }
  }

  def WithPeriodDate(maybeValidatedDate: Option[ValidatedNel[ParseFailure, YearMonth]])(f: YearMonth => F[Response[F]])(implicit F: Monad[F]): F[Response[F]] = {
    maybeValidatedDate match {
      case Some(Invalid(e)) => BadRequest(s"Invalid date: $e")
      case Some(Valid(date)) => f(date)
      case None => BadRequest("startDate parameter missing")
    }
  }

  def WithAccountsOverview()(f: CMAccountsOverview => F[Response[F]])(implicit F: Effect[F]): F[Response[F]] = {
    cmClient.fetchAccountsState().value.flatMap {
      case Right(accountsState) =>
        val allStatements = accountsState.flatMap(_.statements)
        analyticsClient.computeCurrentPeriod(allStatements) flatMap {
          case Some(period) =>
            val accountsOverview = CMAccountsOverview(period, accountsState.map(_.toOverview))
            f(accountsOverview)

          case None =>
            NotFound()

        }

      case Left(otpRequest) => Ok(json"""{ "otpRequired": $otpRequest }""")
    }
  }

  def WithAccountState(accountId: String, periodDate: YearMonth)(f: CMAccountState => F[Response[F]])(implicit F: Effect[F]): F[Response[F]] = {
    analyticsClient.getAccountStateAt(accountId, periodDate).value.flatMap {
      case Some(accountState) =>
        f(accountState)

      case None =>
        cmClient.fetchAccountState(accountId).value.flatMap {
          case Right(Some(accountState)) =>
            f(accountState.since(periodDate.atDay(1)))

          case Right(None) => NotFound()

          case Left(otpRequest) => Ok(json"""{ "otpRequired": $otpRequest }""")
        }
    }
  }
}
