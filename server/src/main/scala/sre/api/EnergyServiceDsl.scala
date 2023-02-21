package sre.api

import java.time.LocalDate
import org.http4s._
import org.http4s.dsl.Http4sDsl
import java.time.format.DateTimeFormatter
import cats.data.{Validated, ValidatedNel}

trait EnergyServiceDsl[F[_]] extends Http4sDsl[F] {

  implicit val dateQueryParamDecoder = new QueryParamDecoder[LocalDate] {
    def decode(
        value: QueryParameterValue
    ): ValidatedNel[ParseFailure, LocalDate] =
      Validated
        .catchNonFatal {
          LocalDate.parse(value.value, DateTimeFormatter.ISO_LOCAL_DATE)
        }
        .leftMap(t =>
          ParseFailure(s"Query decoding LocalDate failed", t.getMessage)
        )
        .toValidatedNel
  }

  object DateFromQueryParamMatcher
      extends OptionalValidatingQueryParamDecoderMatcher[LocalDate]("dateFrom")
  object DateToQueryParamMatcher
      extends OptionalValidatingQueryParamDecoderMatcher[LocalDate]("dateTo")
}
