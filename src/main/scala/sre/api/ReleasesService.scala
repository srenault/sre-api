package sre.api

import org.http4s._
import org.http4s.dsl.Http4sDsl
import cats.effect._
import cats.implicits._
import apk.ApkClient

class ReleasesService[F[_]: Effect](apkClient: ApkClient[F], settings: Settings) extends Http4sDsl[F] {

  val service: HttpService[F] = {
    HttpService[F] {
      case GET -> Root =>
        apkClient.list().flatMap { releases =>
          Ok(releases)
        }

      case GET -> "download" /: rest =>
        val file = rest.toList.mkString("/")
        val stream = apkClient.download(file)
        Ok(stream.map(_.toString)).map(_.putHeaders(
          Header("Content-Disposition", "attachment"),
          Header("filename", file),
          Header("Content-Type", "application/vnd.android.package-archive")
        ))
    }
  }
}
