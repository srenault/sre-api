
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
import org.http4s.client.middleware.{ RequestLogger, ResponseLogger }
import org.http4s.dsl.Http4sDsl
import natchez.Trace
import natchez.http4s.NatchezMiddleware
import natchez.xray.XRay
import org.http4s.ember.client.EmberClientBuilder
import scala.concurrent.ExecutionContext.global
import sre.api.settings.FinanceSettings
import models._
import analytics.AnalyticsClient

object Reindex extends IOLambda[ReindexEvent, ReindexResult] {
  lazy val settings: FinanceSettings = FinanceSettings.fromEnv()

  def handler: Resource[IO, LambdaEnv[IO, ReindexEvent] => IO[Option[ReindexResult]]] = {
    for {
      httpClient <- EmberClientBuilder.default[IO].build
      dbClient <- DBClient.resource[IO](settings)
    } yield { implicit env =>
      val analyticsClient = AnalyticsClient(dbClient, settings)
      analyticsClient.reindex(fromScratch = true).map { result =>
        Some(ReindexResult(result))
      }
    }
  }
}
