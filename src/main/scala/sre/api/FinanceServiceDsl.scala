package sre.api

import java.time.LocalDate
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
import finance.cm.{ CMClient, CMAccount }

trait FinanceServiceDsl[F[_]] extends Http4sDsl[F] {

  def cmClient: CMClient[F]

  implicit val dateQueryParamDecoder = new QueryParamDecoder[LocalDate] {
    def decode(value: QueryParameterValue): ValidatedNel[ParseFailure, LocalDate] =
      Validated
        .catchNonFatal {
          val format = new DateTimeFormatterBuilder()
            .appendPattern("yyyy-MM-dd")
            .toFormatter();
          LocalDate.parse(value.value, format)
        }
        .leftMap(t => ParseFailure(s"Query decoding LocalDate failed", t.getMessage))
        .toValidatedNel
  }

  object DateQueryParamMatcher extends OptionalValidatingQueryParamDecoderMatcher[LocalDate]("startDate")

  object AccountIdVar {
    def unapply(str: String): Option[String] = {
      if (!str.isEmpty) Some(str) else None
    }
  }

  def WithPeriodDate(maybeValidatedDate: Option[ValidatedNel[ParseFailure, LocalDate]])(f: Option[LocalDate] => F[Response[F]])(implicit F: Monad[F]): F[Response[F]] = {
    maybeValidatedDate match {
      case Some(Invalid(e)) => BadRequest(s"Invalid date: $e")
      case Some(Valid(date)) => f(Some(date))
      case None => f(None)
    }
  }

  def WithAccounts()(f: List[CMAccount] => F[Response[F]])(implicit F: Effect[F]): F[Response[F]] = {
    cmClient.fetchAccounts().value.flatMap {
      case Right(accounts) => f(accounts)
      case Left(otpRequest) => Ok(json"""{ "otpRequired": $otpRequest }""")
    }
  }

  def WithAccount(accountId: String)(f: CMAccount => F[Response[F]])(implicit F: Effect[F]): F[Response[F]] = {
    cmClient.fetchAccount(accountId).value.flatMap {
      case Right(Some(account)) => f(account)
      case Right(None) => NotFound()
      case Left(otpRequest) => Ok(json"""{ "otpRequired": $otpRequest }""")
    }
  }
}
