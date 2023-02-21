package sre.api.finance.tasks.models

import java.nio.file.Path
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import sre.api.finance.cm.CMOtpRequest

final case class SnapshotResult(
    fromScratch: Boolean,
    files: Option[List[Path]],
    otpRequest: Option[CMOtpRequest]
)

object SnapshotResult {
  implicit val encoderFiles: Encoder[List[Path]] = new Encoder[List[Path]] {
    final def apply(files: List[Path]): Json = {
      files.map(_.toString).asJson
    }
  }

  implicit val encoder: Encoder[SnapshotResult] = deriveEncoder[SnapshotResult]
}
