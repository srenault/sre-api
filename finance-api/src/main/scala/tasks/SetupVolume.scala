package sre.api
package finance
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
import org.http4s.client.blaze._
import scala.concurrent.ExecutionContext.global
import models._

object SetupVolume extends IOLambda[SetupVolumeEvent, SetupVolumeResult] {
  lazy val settings: Settings = Settings.build()
  lazy val s3Client = S3Client[IO](settings.finance.s3TransactionsBucket)

  def handler: Resource[IO, LambdaEnv[IO, SetupVolumeEvent] => IO[Option[SetupVolumeResult]]] = {
    for {
      httpClient <- BlazeClientBuilder[IO](global).resource
      cmClient <- cm.CMClient.resource(httpClient, settings)
    } yield { implicit env =>
      env.event.flatMap { event =>
        s3Client.ls("", event.continuationToken).map { response =>
          Some(SetupVolumeResult(continuationToken = response.continuationToken))
        }
      }
    }
  }
}
