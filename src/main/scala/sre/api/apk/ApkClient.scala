package sre.api
package apk

import java.time.ZoneId
import cats.effect._
import cats.implicits._
import fs2.Stream
import utils.S3Client

case class ApkClient[F[_]: ConcurrentEffect](s3Client: S3Client[F]) {

  private lazy val KeyReg = {
    val prefix = "^" + s3Client.prefix.map(p => p + "/").getOrElse("")
    (prefix + """(.+)/([^/]+)$""").r
  }

  def list(): F[List[ApkFile]] = {
    s3Client.ls().map { objects =>
      objects.map(obj => obj.getKey -> obj).collect {
        case (KeyReg(branch, name), obj) =>
          val date = obj.getLastModified
            .toInstant()
            .atZone(ZoneId.systemDefault())
            .toLocalDate();

          ApkFile(branch, name, date, obj.getSize)
      }
    }
  }

  def download(file: String): Stream[F, Byte] = {
    s3Client.streamFile(file)
  }
}
