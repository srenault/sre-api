package sre.api
package finance
package tasks

import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import cats.Parallel
import cats.effect.syntax.all._
import io.circe.{ Decoder, Encoder }
import cats.effect._
import cats.implicits._
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
import org.http4s.client.blaze._
import scala.concurrent.ExecutionContext.global
import models._

object SetupVolume extends IOLambda[SetupVolumeEvent, SetupVolumeResult] {
  lazy val settings: Settings = Settings.build()

  lazy val s3Client = S3Client[IO](settings.finance.s3TransactionsBucket)

  implicit def logger[F[_]: Sync]: Logger[F] = Slf4jLogger.getLogger[F]

  def handler: Resource[IO, LambdaEnv[IO, SetupVolumeEvent] => IO[Option[SetupVolumeResult]]] = {
    for {
      httpClient <- BlazeClientBuilder[IO](global).resource
      cmClient <- cm.CMClient.resource(httpClient, settings)
    } yield { implicit env =>
      run(env)
    }
  }

  private def run(env: LambdaEnv[IO, SetupVolumeEvent])(implicit parallel: Parallel[IO], logger: Logger[IO]): IO[Option[SetupVolumeResult]] = {
    for {
      event <- env.event

      _ <- Logger[IO].info(s"Listing transactions to download [continuationToken=${event.continuationToken}]")

      listing <- s3Client.ls("", settings.finance.setupVolume.maxKeys, event.continuationToken)

      _ <- Logger[IO].info(s"Downloading transactions...")

      _ <- listing.objects.map { obj =>
        val destinationPath = settings.finance.transactionsDir.resolve(obj.getKey)
        destinationPath.toFile.mkdirs
        s3Client.downloadFileTo(obj.getKey, destinationPath)
      }.parSequence

      _ <- Logger[IO].info(s"Transactions downloaded [nextContinuationToken=${listing.continuationToken}]")

    } yield Some(SetupVolumeResult(listing.continuationToken))
  }
}
