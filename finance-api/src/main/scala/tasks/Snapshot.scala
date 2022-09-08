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
import models._

object Snapshot extends IOLambda[SnapshotEvent, SnapshotResult] {
  lazy val settings: Settings = Settings.build()

  def handler: Resource[IO, LambdaEnv[IO, SnapshotEvent] => IO[Option[SnapshotResult]]] = {
     for {
       httpClient <- EmberClientBuilder.default[IO].build
      cmClient <- cm.CMClient.resource(httpClient, settings)
    } yield { implicit env =>
        cmClient.fetchAccountsOfxStmTrn() {
          case (accountId, response) =>
            val accountPath = settings.finance.transactionsDir.resolve(accountId)
            ofx.OfxStmTrn.persist(is = response.body, accountPath).map(_ => accountPath)
        }.value.map {
          case Right(files) =>
            SnapshotResult(files = Some(files), otpRequest = None)
          case Left(otpRequest) =>
            SnapshotResult(files = None, otpRequest = Some(otpRequest))
        }.map(Some(_))
    }
  }
}
