package sre.api.finance
package tasks

import java.io.File
import cats.effect.syntax.all._
import cats.effect._
import cats.effect.kernel.Resource
import cats.effect.std.Random
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import feral.lambda._
import feral.lambda.events._
import feral.lambda.http4s._
import org.http4s.client.blaze._
import org.http4s.client._
import org.http4s.client.middleware.{RequestLogger, ResponseLogger}
import org.http4s.dsl.Http4sDsl
import natchez.Trace
import natchez.http4s.NatchezMiddleware
import natchez.xray.XRay
import scala.concurrent.ExecutionContext.Implicits.global
import sre.api.settings.FinanceSettings
import models._

object ResetVolume extends IOLambda[ResetVolumeEvent, ResetVolumeResult] {

  implicit def logger[F[_]: Sync]: Logger[F] = Slf4jLogger.getLogger[F]

  lazy val settings: FinanceSettings = FinanceSettings.fromEnv()

  def handler: Resource[IO, LambdaEnv[IO, ResetVolumeEvent] => IO[
    Option[ResetVolumeResult]
  ]] = {
    for {
      httpClient <- BlazeClientBuilder[IO](global).resource
      cmClient <- cm.CMClient.resource(httpClient, settings)
      dbClient <- DBClient.resource[IO](settings)
      financeTasks = new FinanceTasks(cmClient, dbClient, settings)
    } yield { implicit env =>
      env.event.flatMap { resetVolumeEvent =>
        financeTasks.resetVolume().map(_ => Some(ResetVolumeResult()))
      }
    }
  }
}
