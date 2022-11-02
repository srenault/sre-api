package sre.api.finance
package tasks

import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import cats.effect.syntax.all._
import cats.effect._
import cats.effect.kernel.Resource
import cats.effect.std.Random
import feral.lambda._
import feral.lambda.events._
import feral.lambda.http4s._
import org.http4s.client.middleware.{RequestLogger, ResponseLogger}
import org.http4s.dsl.Http4sDsl
import org.http4s.client.blaze._
import org.http4s.client._
import natchez.Trace
import natchez.http4s.NatchezMiddleware
import natchez.xray.XRay
import scala.concurrent.ExecutionContext.Implicits.global
import sre.api.settings.FinanceSettings
import models._
import analytics.AnalyticsClient

object Reindex extends IOLambda[ReindexEvent, ReindexResult] {
  lazy val settings: FinanceSettings = FinanceSettings.fromEnv()

  implicit def logger[F[_]: Sync]: Logger[F] = Slf4jLogger.getLogger[F]

  def handler: Resource[IO, LambdaEnv[IO, ReindexEvent] => IO[
    Option[ReindexResult]
  ]] = {
    for {
      httpClient <- BlazeClientBuilder[IO](global).resource
      dbClient <- DBClient.resource[IO](settings)
      cmClient <- cm.CMClient.resource(httpClient, settings)
    } yield { implicit env =>
      val financeTasks = new FinanceTasks(cmClient, dbClient, settings)
      env.event.flatMap { event =>
        financeTasks.reindex(fromScratch = event.fromScratch).map { result =>
          Some(ReindexResult(result))
        }
      }
    }
  }
}
