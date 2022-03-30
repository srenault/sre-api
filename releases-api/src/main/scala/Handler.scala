package sre.api.releases

import cats.effect.syntax.all._
import io.circe.Decoder
import io.circe.Encoder

import cats.effect._
import cats.effect.kernel.Resource
import cats.effect.std.Random
import feral.lambda._
import feral.lambda.events._
import feral.lambda.http4s._
import org.http4s.dsl.Http4sDsl

object Handler extends IOLambda[ApiGatewayProxyEventV2, ApiGatewayProxyStructuredResultV2] {
  val settings: Settings = Settings.build()

  def handler: Resource[IO, LambdaEnv[IO, ApiGatewayProxyEventV2] => IO[Option[ApiGatewayProxyStructuredResultV2]]] = {
    Resource.eval(Random.scalaUtilRandom[IO]).map { _ => implicit env =>
      val s3Client = S3Client[IO](settings.s3)
      val apkClient = ApkClient(s3Client)
      val releasesClient = ReleasesClient(apkClient)
      val service = new ReleasesService(releasesClient, settings)
      ApiGatewayProxyHandler(service.routes)
    }
  }
}
