package sre.api
package finance
package tasks

import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import cats.Parallel
import cats.effect.syntax.all._
import io.circe.{Decoder, Encoder}
import cats.effect._
import cats.implicits._
import cats.effect.kernel.Resource
import cats.effect.std.Random
import feral.lambda._
import feral.lambda.events._
import feral.lambda.http4s._
import org.http4s.client._
import org.http4s.ember.client._
import org.http4s.client.middleware.{RequestLogger, ResponseLogger}
import org.http4s.dsl.Http4sDsl
import natchez.Trace
import natchez.http4s.NatchezMiddleware
import natchez.xray.XRay
import sre.api.settings.FinanceSettings
import models._

object SetupVolume extends IOLambda[SetupVolumeEvent, SetupVolumeResult] {
  implicit def logger[F[_]: Sync]: Logger[F] = Slf4jLogger.getLogger[F]

  def handler: Resource[IO, LambdaEnv[IO, SetupVolumeEvent] => IO[
    Option[SetupVolumeResult]
  ]] = {
    for {
      settings <- Resource.eval(FinanceSettings.fromEnv[IO]())
      httpClient <- EmberClientBuilder.default[IO].build
      dbClient <- DBClient.resource[IO](settings)
      cmClient <- cm.CMClient.resource(httpClient, settings)
    } yield { implicit env =>
      val financeTasks = new FinanceTasks(cmClient, dbClient, settings)
      for {
        event <- env.event

        nextContinuationToken <- financeTasks.setupVolume(
          event.continuationToken
        )

      } yield Some(SetupVolumeResult(nextContinuationToken))
    }
  }
}
