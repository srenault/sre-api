package sre.api.releases

import cats.effect._
import cats.implicits._

case class ReleasesClient[F[_]: Async](apkClient: ApkClient[F]) {

  def list()(implicit settings: Settings): F[List[Release]] = {
    apkClient.list().map { apkFiles =>
      apkFiles.groupBy(_.branch).toList.map {
        case (branch, files) =>
          val versions = files.map { file =>
            Version(sha1 = file.sha1.take(7), date = file.lastModified, url = file.url)
          }
          Release(name = branch, versions)
      }
    }
  }
}
