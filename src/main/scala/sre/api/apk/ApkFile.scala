package sre.api
package apk

import java.time.LocalDate
import io.circe.Encoder
import io.circe.generic.semiauto._

case class ApkFile(branch: String, name: String, lastModified: LocalDate, size: Long)

object ApkFile {

  implicit val encoder: Encoder[ApkFile] = deriveEncoder[ApkFile]
}
