package sre.api

import cats.Parallel
import cats.effect._
import org.http4s.server.blaze._
import org.http4s.client.blaze._
import org.http4s.server.Router
import org.http4s.implicits._
import org.http4s.client.middleware.{ RequestLogger, ResponseLogger }
import transport.train.TrainClient
import transport.subway.SubwayClient
import domoticz.DomoticzClient
import weather.WeatherClient
import utils.S3Client
import energy.EnergyClient
import releases.{ ApkClient, ReleasesClient }
import scala.concurrent.ExecutionContext.global

object Server extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    ServerStream.stream[IO].compile.drain.as(ExitCode.Success)
}

object ServerStream {

  def trainService[F[_]: Effect](trainClient: TrainClient[F], settings: Settings) =
    new TrainService[F](trainClient, settings).service

  def subwayService[F[_]: Effect](subwayClient: SubwayClient[F], settings: Settings) =
    new SubwayService[F](subwayClient, settings).service

  def energyService[F[_]: Effect](energyClient: EnergyClient[F], settings: Settings) =
    new EnergyService[F](energyClient, settings).service


  def weatherService[F[_]: Effect](weatherClient: WeatherClient[F], settings: Settings) =
    new WeatherService[F](weatherClient, settings).service

  def releasesService[F[_]: Effect](releasesClient: ReleasesClient[F], settings: Settings) =
    new ReleasesService[F](releasesClient, settings).service

  def stream[F[_] : Timer : ContextShift : ConcurrentEffect : Parallel] = {
    Settings.load() match {
      case Right(settings) =>
        for {
          httpClient <- {
            BlazeClientBuilder(global).stream.map { client =>
              val updatedClient = if (settings.httpClient.logRequest) {
                RequestLogger(logHeaders = true, logBody = true)(client)
              } else client

              if (settings.httpClient.logResponse) {
                ResponseLogger(logHeaders = true, logBody = true)(updatedClient)
              } else updatedClient
            }
          }

          trainClient <- TrainClient.stream[F](httpClient, settings)

          subwayClient = SubwayClient[F](trainClient)

          domoticzClient <- DomoticzClient.stream[F](httpClient, settings.domoticz)

          _ <- domoticzClient.wsConnect

          energyClient = EnergyClient[F](domoticzClient, settings)


          weatherClient = WeatherClient[F](httpClient, settings.weather)

          s3Client = S3Client[F](settings.apk.s3)

          apkClient = ApkClient(s3Client)

          releasesClient = ReleasesClient(apkClient)

          httpApp = Router(
            "/api/transport/train" -> trainService(trainClient, settings),
            "/api/transport/subway" -> subwayService(subwayClient, settings),
            "/api/energy" -> energyService(energyClient, settings),
            "/api/weather" -> weatherService(weatherClient, settings),
            "/api/releases" -> releasesService(releasesClient, settings)
          ).orNotFound

          R <- BlazeServerBuilder[F](global)
                 .bindHttp(settings.httpPort, "0.0.0.0")
                 .withHttpApp(httpApp)
                 .serve
        } yield R

      case Left(error) =>
        sys.error(s"Malformed configuration file $error")
    }
  }
}
