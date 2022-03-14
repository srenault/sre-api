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
import finance.icompta.IComptaClient
import finance.cm.CMClient
import domoticz.DomoticzClient
import heaters.HeatersClient
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

  def financeService[F[_]: ConcurrentEffect : Timer : ContextShift](icomptaClient: IComptaClient[F], cmClient: CMClient[F], dbClient: DBClient[F], settings: Settings) =
    new FinanceService[F](icomptaClient, cmClient, dbClient, settings).service

  def energyService[F[_]: Effect](energyClient: EnergyClient[F], settings: Settings) =
    new EnergyService[F](energyClient, settings).service

  def heatersService[F[_]: Effect](heatersClient: HeatersClient[F], settings: Settings) =
    new HeatersService[F](heatersClient, settings).service

  def weatherService[F[_]: Effect](weatherClient: WeatherClient[F], settings: Settings) =
    new WeatherService[F](weatherClient, settings).service

  def releasesService[F[_]: Effect](releasesClient: ReleasesClient[F], settings: Settings) =
    new ReleasesService[F](releasesClient, settings).service

  def stream[F[_] : Timer : ContextShift : ConcurrentEffect : Parallel] = {
    Settings.load() match {
      case Right(settings) =>
        for {
          dbClient <- DBClient.stream[F](settings)

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

          icomptaClient <- IComptaClient.stream[F](settings)

          cmClient <- CMClient.stream[F](httpClient, settings)

          domoticzClient <- DomoticzClient.stream[F](httpClient, settings.domoticz)

          _ <- domoticzClient.wsConnect

          energyClient = EnergyClient[F](domoticzClient, settings)

          heatersClient = HeatersClient[F](httpClient, settings.heaters)

          weatherClient = WeatherClient[F](httpClient, settings.weather)

          s3Client = S3Client[F](settings.apk.s3)

          apkClient = ApkClient(s3Client)

          releasesClient = ReleasesClient(apkClient)

          httpApp = Router(
            "/api/transport/train" -> trainService(trainClient, settings),
            "/api/transport/subway" -> subwayService(subwayClient, settings),
            "/api/finance" -> financeService(icomptaClient, cmClient, dbClient, settings),
            "/api/energy" -> energyService(energyClient, settings),
            "/api/heaters" -> heatersService(heatersClient, settings),
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
