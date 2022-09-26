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

object CheckOtp extends IOLambda[CheckOtpEvent, CheckOtpResult] {
  lazy val settings: FinanceSettings = FinanceSettings.fromEnv()

  def handler: Resource[IO, LambdaEnv[IO, CheckOtpEvent] => IO[Option[CheckOtpResult]]] = {
    for {
      httpClient <- EmberClientBuilder.default[IO].build
      cmClient <- cm.CMClient.resource(httpClient, settings)
    } yield { implicit env =>
        env.event.flatMap { checkOtpEvent =>
          cmClient.checkOtpStatus(checkOtpEvent.otpRequest.transactionId).map { status =>
            Some(CheckOtpResult(status))
          }
        }
    }
  }
}
