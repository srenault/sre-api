package sre.api

import org.http4s.dsl.Http4sDsl

trait WeatherServiceDsl[F[_]] extends Http4sDsl[F] {

  object LatitudeQueryParamMatcher
      extends QueryParamDecoderMatcher[Double]("latitude")

  object LongitudeQueryParamMatcher
      extends QueryParamDecoderMatcher[Double]("longitude")
}
