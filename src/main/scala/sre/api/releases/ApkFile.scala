package sre.api
package releases

import java.time.LocalDateTime
import cats.effect._
import io.circe.Encoder
import io.circe.generic.semiauto._
import org.http4s.EntityEncoder
import org.http4s.circe._

case class ApkFile(branch: String, name: String, lastModified: LocalDateTime, size: Long, url: String) {
  lazy val sha1: String = name match {
    case ApkFile.SHA1Reg(sha1) => sha1
  }
}

object ApkFile {

  val SHA1Reg = """^sreapp-(.+)\.apk$""".r

  implicit val encoder: Encoder[ApkFile] = deriveEncoder[ApkFile]
  implicit def entitiesEncoder[F[_]: Effect]: EntityEncoder[F, List[ApkFile]] = jsonEncoderOf[F, List[ApkFile]]
}
