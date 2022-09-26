package sre.api.finance.cm

import java.io._
import java.nio.file.{ Files, Path, Paths }
import cats.effect._
import cats.data.OptionT
import cats.implicits._
import io.circe.Error
import io.circe.parser._

case class CMOtpSessionFile[F[_] : Sync](path: Path) {

  def ensurePath: Path = {
    val f = path.toFile
    if(!f.exists) f.createNewFile
    path
  }

  private def resourceReader: Resource[F, BufferedReader] =
    Resource.make {
      Sync[F].delay(Files.newBufferedReader(ensurePath))
    } { reader =>
      Sync[F].delay(reader.close()).handleErrorWith(_ => Sync[F].unit)
    }

  private def resourceWriter: Resource[F, BufferedWriter] =
    Resource.make {
      Sync[F].delay(Files.newBufferedWriter(ensurePath))
    } { writer =>
      Sync[F].delay(writer.close()).handleErrorWith(_ => Sync[F].unit)
    }

  def get: OptionT[F, Either[Error, CMValidOtpSession]] =
    OptionT(resourceReader.use { reader =>
      Sync[F].blocking {
        Option(reader.readLine).filterNot(_.trim.isEmpty).map { line =>
          parse(line).flatMap(_.as[CMValidOtpSession])
        }
      }
    })

  def set(otpSession: CMValidOtpSession): F[Unit] =
    resourceWriter.use { writer =>
      Sync[F].blocking(writer.write(CMValidOtpSession.encoder(otpSession).noSpaces))
    }

  def delete(): F[Boolean] = {
    Sync[F].blocking(Files.deleteIfExists(path))
  }
}
