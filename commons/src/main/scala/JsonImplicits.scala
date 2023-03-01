package sre.api

import java.io.File
import java.nio.file.Path
import org.http4s.Uri
import java.nio.file.Paths
import io.circe._

object JsonImplicits {

  implicit val UriDecoder: Decoder[Uri] = new Decoder[Uri] {
    final def apply(c: HCursor): Decoder.Result[Uri] =
      c.as[String].flatMap { s =>
        Uri.fromString(s).left.map { error =>
          DecodingFailure(error.message, c.history)
        }
      }
  }

  implicit val FileDecoder: Decoder[File] = new Decoder[File] {
    final def apply(c: HCursor): Decoder.Result[File] =
      c.as[String].flatMap { s =>
        val f = new File(s)
        if (f.exists) Right(f) else Left {
          DecodingFailure(s"$s file doesn't exists", c.history)
        }
      }
  }

  implicit val PathDecoder: Decoder[Path] = new Decoder[Path] {
    final def apply(c: HCursor): Decoder.Result[Path] =
      c.as[String].map { s =>
        Paths.get(s)
      }
  }
}
