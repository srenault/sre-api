package sre.api
package apk

import java.time.LocalDate
import cats.effect._
import io.circe.Encoder
import io.circe.generic.semiauto._
import org.http4s.EntityEncoder
import org.http4s.circe._

case class ApkFile(branch: String, name: String, lastModified: LocalDate, size: Long)

object ApkFile {

  implicit val encoder: Encoder[ApkFile] = deriveEncoder[ApkFile]
  implicit def entitiesEncoder[F[_]: Effect]: EntityEncoder[F, List[ApkFile]] = jsonEncoderOf[F, List[ApkFile]]
}
