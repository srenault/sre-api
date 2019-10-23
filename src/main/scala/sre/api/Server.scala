package sre.api

import cats.effect._
import cats.implicits._
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.client.blaze._
import org.http4s.client.middleware.RequestLogger
import transport.train.TrainClient
import transport.subway.SubwayClient
import finance.icompta.IComptaClient
import finance.cm.CMClient
import domoticz.DomoticzClient
import weather.WeatherClient
import utils.S3Client
import releases.{ ApkClient, ReleasesClient }

object Server extends IOApp {

  def run(args: List[String]) =
    ServerStream.stream[IO].compile.drain.as(ExitCode.Success)
}

object ServerStream {

  def trainService[F[_]: Effect](trainClient: TrainClient[F], settings: Settings) =
    new TrainService[F](trainClient, settings).service

  def subwayService[F[_]: Effect](subwayClient: SubwayClient[F], settings: Settings) =
    new SubwayService[F](subwayClient, settings).service

  def financeService[F[_]: ConcurrentEffect : Timer](icomptaClient: IComptaClient[F], cmClient: CMClient[F], dbClient: DBClient[F], settings: Settings) =
    new FinanceService[F](icomptaClient, cmClient, dbClient, settings).service

  def energyService[F[_]: Effect](domoticzClient: DomoticzClient[F], settings: Settings) =
    new EnergyService[F](domoticzClient, settings).service

  def weatherService[F[_]: Effect](weatherClient: WeatherClient[F], settings: Settings) =
    new WeatherService[F](weatherClient, settings).service

  def releasesService[F[_]: Effect](releasesClient: ReleasesClient[F], settings: Settings) =
    new ReleasesService[F](releasesClient, settings).service

  def stream[F[_] : Timer : ContextShift : ConcurrentEffect] = {
    Settings.load() match {
      case Right(settings) =>
        for {
          dbClient <- DBClient.stream[F](settings)
          httpClient <- Http1Client.stream[F]().map(RequestLogger(true, true))
          trainClient <- TrainClient.stream[F](httpClient, settings)
          subwayClient = SubwayClient[F](trainClient)
          icomptaClient <- IComptaClient.stream[F](settings)
          cmClient <- CMClient.stream[F](httpClient, settings)
          domoticzClient = DomoticzClient[F](httpClient, settings.domoticz)
          weatherClient = WeatherClient[F](httpClient, settings.weather)
          s3Client = S3Client[F](settings.apk.s3)
          apkClient = ApkClient(s3Client)
          releasesClient = ReleasesClient(apkClient)
          R <- BlazeBuilder[F].bindHttp(settings.httpPort, "0.0.0.0")
                              .mountService(trainService(trainClient, settings), "/api/transport/train")
                              .mountService(subwayService(subwayClient, settings), "/api/transport/subway")
                              .mountService(financeService(icomptaClient, cmClient, dbClient, settings), "/api/finance")
                              .mountService(energyService(domoticzClient, settings), "/api/energy")
                              .mountService(weatherService(weatherClient, settings), "/api/weather")
                              .mountService(releasesService(releasesClient, settings), "/api/releases")
                              .serve
        } yield R

      case Left(error) =>
        sys.error(s"Malformed configuration file $error")
    }
  }
}
