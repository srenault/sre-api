package sre.dashboard


import java.time.LocalDate
import java.time.temporal.ChronoField
import java.time.format.DateTimeFormatterBuilder
import cats.data.{ Validated, ValidatedNel }
import org.http4s._
import org.http4s.dsl.Http4sDsl

trait FinanceServiceDsl[F[_]] extends Http4sDsl[F] {

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
    def unapply(str: String)(implicit settings: Settings): Option[String] = {
      if (!str.isEmpty) Some(str) else None
    }
  }
}
