package sre.api.releases

import org.http4s.dsl.Http4sDsl

trait ReleaseServiceDsl[F[_]] extends Http4sDsl[F] {

  object DownloadQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Boolean]("download")
}
