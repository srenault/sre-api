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
import org.http4s.client._
import org.http4s.ember.client._
import natchez.Trace
import natchez.http4s.NatchezMiddleware
import natchez.xray.XRay
import sre.api.settings.FinanceSettings
import models._

object ImportStatements
    extends IOLambda[ImportStatementsEvent, ImportStatementsResult] {

  implicit def logger[F[_]: Sync]: Logger[F] = Slf4jLogger.getLogger[F]

  def handler: Resource[IO, LambdaEnv[IO, ImportStatementsEvent] => IO[
    Option[ImportStatementsResult]
  ]] = {
    for {
      settings <- Resource.eval(FinanceSettings.fromEnv[IO]())
      httpClient <- EmberClientBuilder.default[IO].build
      dbClient <- DBClient.resource[IO](settings)
      cmClient <- cm.CMClient.resource(httpClient, settings)
    } yield { implicit env =>
      val financeTasks = new FinanceTasks(cmClient, dbClient, settings)
      financeTasks.importStatements().value.map { result =>
        Some(ImportStatementsResult(result.left.toOption))
      }
    }
  }
}
