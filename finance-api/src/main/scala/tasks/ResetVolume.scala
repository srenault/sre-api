package sre.api.finance
package tasks

import java.io.File
import cats.effect.syntax.all._
import io.circe.Decoder
import io.circe.Encoder

import cats.effect._
import cats.effect.kernel.Resource
import cats.effect.std.Random
import feral.lambda._
import feral.lambda.events._
import feral.lambda.http4s._
import org.http4s.client.middleware.{ RequestLogger, ResponseLogger }
import org.http4s.dsl.Http4sDsl
import natchez.Trace
import natchez.http4s.NatchezMiddleware
import natchez.xray.XRay
import org.http4s.ember.client.EmberClientBuilder
import scala.concurrent.ExecutionContext.global
import sre.api.settings.FinanceSettings
import models._

object ResetVolume extends IOLambda[ResetVolumeEvent, ResetVolumeResult] {
  lazy val settings: FinanceSettings = FinanceSettings.fromEnv()

  def handler: Resource[IO, LambdaEnv[IO, ResetVolumeEvent] => IO[Option[ResetVolumeResult]]] = {
    for {
      httpClient <- EmberClientBuilder.default[IO].build
      cmClient <- cm.CMClient.resource(httpClient, settings)
    } yield { implicit env =>
        env.event.flatMap { resetVolumeEvent =>
          deleteRecursively[IO](settings.transactionsDir.toFile).map { _ =>
            Some(ResetVolumeResult())
          }
        }
    }
  }

  def deleteRecursively[F[_]](file: File)(implicit F: Sync[F]): F[Unit] =
    F.blocking {
      if (file.isDirectory) {
        file.listFiles.foreach(f => deleteRecursively(f))
      }
      if (file.exists && !file.delete) {
        throw new Exception(s"Unable to delete ${file.getAbsolutePath}")
      }
    }
}
