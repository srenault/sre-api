package sre.api.releases

import org.http4s._
import org.http4s.headers._
import cats.effect._
import cats.implicits._

class ReleasesService[F[_]: Concurrent](releasesClient: ReleasesClient[F], settings: Settings) extends ReleaseServiceDsl[F] {

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "releases" =>
      releasesClient.list()(settings).flatMap { releases =>
        Ok(releases, `Content-Type`(MediaType.application.json, Charset.`UTF-8`))
      }

    case GET -> "releases" /: "download" /: rest :? DownloadQueryParamMatcher(maybeDownload) =>
      val download = maybeDownload getOrElse false
      val key = rest.toString
      val name = rest.segments.last.decoded()

      if (download) {
        val stream = releasesClient.apkClient.download(key)
        Ok(stream).map(_.putHeaders(
          Header("Content-Disposition", "attachment"),
          Header("filename", name),
          Header("Content-Type", "application/vnd.android.package-archive")
        ))
      } else {
        val url = s"${settings.advertisedAddress}/api/releases/download/$key?download=true"
        val html = s"""
          |<html>
          |  <body>
          |    <h1>Download</h1>
          |    <a href="$url">$name</a>
          |  </body>
          |</html>
          """.stripMargin
        Ok(html, `Content-Type`(MediaType.text.html, Charset.`UTF-8`))
      }
  }
}
