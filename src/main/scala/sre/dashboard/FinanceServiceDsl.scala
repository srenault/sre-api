package sre.dashboard


import java.time.LocalDate
import java.time.temporal.ChronoField
import java.time.format.DateTimeFormatterBuilder
import cats._
import cats.implicits._
import cats.data.{ Validated, ValidatedNel }
import cats.data.Validated.{ Invalid, Valid }
import org.http4s._
import org.http4s.dsl.Http4sDsl
import finance._

trait FinanceServiceDsl[F[_]] extends Http4sDsl[F] {

  def cmClient: CMClient[F]

  implicit val dateQueryParamDecoder = new QueryParamDecoder[LocalDate] {
    def decode(value: QueryParameterValue): ValidatedNel[ParseFailure, LocalDate] =
      Validated
        .catchNonFatal {
          val format = new DateTimeFormatterBuilder()
            .appendPattern("yyyy-MM")
            .parseDefaulting(ChronoField.DAY_OF_MONTH, 1)
            .toFormatter();
          LocalDate.parse(value.value, format)
        }
        .leftMap(t => ParseFailure(s"Query decoding LocalDate failed", t.getMessage))
        .toValidatedNel
  }

  object DateQueryParamMatcher extends OptionalValidatingQueryParamDecoderMatcher[LocalDate]("date")

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

  def WithAccount(accountId: String)(f: CMAccount => F[Response[F]])(implicit F: Monad[F]): F[Response[F]] = {
    cmClient.fetchAccount(accountId).flatMap {
      case Some(account) => f(account)
      case None => NotFound()
    }
  }
}
