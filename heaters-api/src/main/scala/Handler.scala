package sre.api.heaters

import cats.effect.syntax.all._
import io.circe.Decoder
import io.circe.Encoder
import cats.effect._
import cats.effect.kernel.Resource
import cats.effect.std.Random
import feral.lambda._
import feral.lambda.events._
import feral.lambda.http4s._
import org.http4s.ember.client._
import org.http4s.client._
import org.http4s.client.middleware.{RequestLogger, ResponseLogger}
import org.http4s.dsl.Http4sDsl
import natchez.Trace
import natchez.http4s.NatchezMiddleware
import natchez.xray.XRay
import sre.api.settings.HeatersSettings

object Handler
    extends IOLambda[
      ApiGatewayProxyEventV2,
      ApiGatewayProxyStructuredResultV2
    ] {

  def handler: Resource[IO, LambdaEnv[IO, ApiGatewayProxyEventV2] => IO[
    Option[ApiGatewayProxyStructuredResultV2]
  ]] = {
    for {
      settings <- Resource.eval(HeatersSettings.fromEnv[IO]())
      entrypoint <- Resource
        .eval(Random.scalaUtilRandom[IO])
        .flatMap(implicit r => XRay.entryPoint[IO]())
      httpClient <- EmberClientBuilder.default[IO].build
    } yield { implicit env =>
      TracedHandler(entrypoint) { implicit trace =>
        val tracedHttpClient = NatchezMiddleware.client(httpClient)
        val heatersClient = HeatersClient(tracedHttpClient, settings)
        val service = new HeatersHttpService(heatersClient, settings)
        ApiGatewayProxyHandler(service.routes)
      }
    }
  }
}
