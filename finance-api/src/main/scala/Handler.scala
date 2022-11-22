package sre.api.finance

import scala.concurrent.ExecutionContext.Implicits.global
import cats.effect.syntax.all._
import io.circe.Decoder
import io.circe.Encoder
import cats.effect._
import cats.effect.kernel.Resource
import cats.effect.std.Random
import feral.lambda._
import feral.lambda.events._
import feral.lambda.http4s._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.http4s.client.blaze._
import org.http4s.client._
import org.http4s.client.middleware.{RequestLogger, ResponseLogger}
import org.http4s.dsl.Http4sDsl
import natchez.Trace
import natchez.http4s.NatchezMiddleware
import natchez.xray.XRay
import sre.api.settings.FinanceSettings

object Handler
    extends IOLambda[
      ApiGatewayProxyEventV2,
      ApiGatewayProxyStructuredResultV2
    ] {
  implicit def logger[F[_]: Sync]: Logger[F] = Slf4jLogger.getLogger[F]

  def handler: Resource[IO, LambdaEnv[IO, ApiGatewayProxyEventV2] => IO[
    Option[ApiGatewayProxyStructuredResultV2]
  ]] = {
    for {
      settings <- Resource.eval(FinanceSettings.fromEnv[IO]())
      entrypoint <- Resource
        .eval(Random.scalaUtilRandom[IO])
        .flatMap(implicit r => XRay.entryPoint[IO]())
      httpClient <- BlazeClientBuilder[IO](global).resource
      dbClient <- DBClient.resource[IO](settings)
    } yield { implicit env =>
      TracedHandler(entrypoint) { implicit trace =>
        val tracedHttpClient = NatchezMiddleware.client(httpClient)
        cm.CMClient.resource(tracedHttpClient, settings).use { cmClient =>
          val service = new FinanceHttpService(cmClient, dbClient, settings)
          ApiGatewayProxyHandler(service.routes)
        }

      }
    }
  }
}
