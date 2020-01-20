package sre.api

import org.http4s._
import org.http4s.headers._
import cats.effect._
import cats.implicits._
import releases.ReleasesClient

class ReleasesService[F[_]: Effect](releasesClient: ReleasesClient[F], settings: Settings) extends ReleaseServiceDsl[F] {

  val service: HttpService[F] =
    HttpService[F] {
      case GET -> Root =>
        releasesClient.list()(settings).flatMap { releases =>
          Ok(releases)
        }

      case GET -> "download" /: rest :? DownloadQueryParamMatcher(maybeDownload) =>
        val download = maybeDownload getOrElse false
        val key = rest.toList.mkString("/")
        val name = rest.toList.last

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
          Ok(html, `Content-Type`(MediaType.text.html))
        }
    }
}
