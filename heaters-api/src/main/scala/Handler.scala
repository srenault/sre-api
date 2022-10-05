package sre.api.heaters

import cats.effect.syntax.all._
import io.circe.Decoder
import io.circe.Encoder
import scala.concurrent.ExecutionContext.Implicits.global
import cats.effect._
import cats.effect.kernel.Resource
import cats.effect.std.Random
import feral.lambda._
import feral.lambda.events._
import feral.lambda.http4s._
import org.http4s.client.blaze._
import org.http4s.client._
import org.http4s.client.middleware.{ RequestLogger, ResponseLogger }
import org.http4s.dsl.Http4sDsl
import natchez.Trace
import natchez.http4s.NatchezMiddleware
import natchez.xray.XRay
import sre.api.settings.HeatersSettings

object Handler extends IOLambda[ApiGatewayProxyEventV2, ApiGatewayProxyStructuredResultV2] {
  val settings: HeatersSettings = HeatersSettings.fromEnv()

  def handler: Resource[IO, LambdaEnv[IO, ApiGatewayProxyEventV2] => IO[Option[ApiGatewayProxyStructuredResultV2]]] = {
    for {
      entrypoint <- Resource.eval(Random.scalaUtilRandom[IO]).flatMap(implicit r => XRay.entryPoint[IO]())
      httpClient <- BlazeClientBuilder[IO](global).resource
    } yield { implicit env =>
      import cats.effect.unsafe.implicits.global
      TracedHandler(entrypoint) { implicit trace =>
        val tracedHttpClient = NatchezMiddleware.client(httpClient)
        val heatersClient = HeatersClient(tracedHttpClient, settings)
        val service = new HeatersHttpService(heatersClient, settings)
        ApiGatewayProxyHandler(service.routes)
      }
    }
  }
}
