package sre.api.releases

import java.time.ZoneId
import cats.effect._
import cats.implicits._
import fs2.Stream

case class ApkClient[F[_]: Async](s3Client: S3Client[F]) {

  private lazy val KeyReg = {
    val prefix = "^" + s3Client.prefix.map(p => p + "/").getOrElse("")
    (prefix + """(.+)/([^/]+)$""").r
  }

  def list()(implicit settings: Settings): F[List[ApkFile]] = {
    s3Client.ls().map { objects =>
      objects.map(obj => obj.getKey -> obj).collect {
        case (KeyReg(branch, name), obj) =>
          val date = obj.getLastModified
            .toInstant()
            .atZone(ZoneId.systemDefault())
            .toLocalDateTime();

          val url = s"${settings.advertisedAddress}/api/releases/download/$branch/$name"

          ApkFile(branch, name, date, obj.getSize, url)
      }
    }
  }

  def download(file: String): Stream[F, Byte] = {
    s3Client.streamFile(file)
  }
}
