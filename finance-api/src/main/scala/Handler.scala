package sre.api.finance

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
import org.http4s.client._
import org.http4s.client.middleware.{RequestLogger, ResponseLogger}
import org.http4s.ember.client._
import org.http4s.dsl.Http4sDsl
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
      httpClient <- EmberClientBuilder.default[IO].build
      dbClient <- DBClient.resource[IO](settings)
    } yield { implicit env =>
      cm.CMClient.resource(httpClient, settings).use { cmClient =>
        val service = new FinanceHttpService(cmClient, dbClient, settings)
        ApiGatewayProxyHandler(service.routes)
      }
    }
  }
}
