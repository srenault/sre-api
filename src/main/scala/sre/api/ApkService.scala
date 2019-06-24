package sre.api

import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import io.circe.literal._
import cats.effect._
import cats.implicits._
import apk.ApkClient

class ApkService[F[_]: Effect](apkClient: ApkClient[F], settings: Settings) extends Http4sDsl[F] {

  val service: HttpService[F] = {
    HttpService[F] {
      case GET -> Root =>
        apkClient.list().flatMap { apks =>
          Ok(json"""{ "result": $apks }""")
        }

      case GET -> Root / "download" / branch / file =>
        val stream = apkClient.download(branch, file)
        Ok(stream.map(_.toString)).map(_.putHeaders(
          Header("Content-Disposition", "attachment"),
          Header("filename", file),
          Header("Content-Type", "application/vnd.android.package-archive")
        ))
    }
  }
}
