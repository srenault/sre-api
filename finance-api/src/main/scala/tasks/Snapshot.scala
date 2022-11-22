package sre.api.finance
package tasks

import cats.effect.syntax.all._
import io.circe.Decoder
import io.circe.Encoder
import cats.effect._
import cats.effect.kernel.Resource
import cats.effect.std.Random
import feral.lambda._
import feral.lambda.events._
import feral.lambda.http4s._
import org.http4s.client.middleware.{RequestLogger, ResponseLogger}
import org.http4s.dsl.Http4sDsl
import natchez.Trace
import natchez.http4s.NatchezMiddleware
import natchez.xray.XRay
import org.http4s.client.blaze._
import org.http4s.client._
import scala.concurrent.ExecutionContext.Implicits.global
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import sre.api.settings.FinanceSettings
import models._

object Snapshot extends IOLambda[SnapshotEvent, SnapshotResult] {
  implicit def logger[F[_]: Sync]: Logger[F] = Slf4jLogger.getLogger[F]

  def handler: Resource[IO, LambdaEnv[IO, SnapshotEvent] => IO[
    Option[SnapshotResult]
  ]] = {
    for {
      settings <- Resource.eval(FinanceSettings.fromEnv[IO]())
      httpClient <- BlazeClientBuilder[IO](global).resource
      cmClient <- cm.CMClient.resource(httpClient, settings)
      dbClient <- DBClient.resource[IO](settings)
    } yield { implicit env =>
      val financeTasks = new FinanceTasks(cmClient, dbClient, settings)
      env.event.flatMap { event =>
        financeTasks
          .snapshot()
          .value
          .map {
            case Right(files) =>
              SnapshotResult(
                fromScratch = event.fromScratch,
                files = Some(files),
                otpRequest = None
              )
            case Left(otpRequest) =>
              SnapshotResult(
                fromScratch = event.fromScratch,
                files = None,
                otpRequest = Some(otpRequest)
              )
          }
          .map(Some(_))
      }
    }
  }
}
