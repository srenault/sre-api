package sre.api

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import org.http4s._
import org.http4s.dsl.Http4sDsl

trait TrainServiceDsl[F[_]] extends Http4sDsl[F] {

  object LatitudeQueryParamMatcher
      extends QueryParamDecoderMatcher[Double]("latitude")

  object LongitudeQueryParamMatcher
      extends QueryParamDecoderMatcher[Double]("longitude")

  object DistanceQueryParamMatcher
      extends QueryParamDecoderMatcher[Int]("distance")

  implicit val dateQueryParamDecoder: QueryParamDecoder[Option[ZonedDateTime]] =
    QueryParamDecoder[String].map { dateStr =>
      scala.util.Try {
        ZonedDateTime.parse(dateStr, DateTimeFormatter.ISO_OFFSET_DATE_TIME)
      }.toOption
    }

  object DateQueryParamMatcher
      extends QueryParamDecoderMatcher[Option[ZonedDateTime]]("date")
}
