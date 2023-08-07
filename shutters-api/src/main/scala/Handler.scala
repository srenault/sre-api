package sre.api.shutters

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
import org.http4s.ember.client._
import org.http4s.dsl.Http4sDsl
import sre.api.domoticz.DomoticzSettings
import sre.api.domoticz.DomoticzClient
import sre.api.settings.ShuttersSettings

object Handler
    extends IOLambda[
      ApiGatewayProxyEventV2,
      ApiGatewayProxyStructuredResultV2
    ] {

  def handler: Resource[IO, LambdaEnv[IO, ApiGatewayProxyEventV2] => IO[
    Option[ApiGatewayProxyStructuredResultV2]
  ]] = {
    for {
      domoticzSettings <- Resource.eval(DomoticzSettings.fromEnv[IO]())
      shuttersSettings <- Resource.eval(ShuttersSettings.fromEnv[IO]())
      httpClient <- EmberClientBuilder.default[IO].build
      domoticzClient <- DomoticzClient.resource(httpClient, domoticzSettings)
    } yield { implicit env =>
      val service = new ShuttersHttpService(domoticzClient, shuttersSettings)
      ApiGatewayProxyHandler(service.routes)
    }
  }
}
