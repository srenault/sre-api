package sre.dashboard

import cats.effect._
import cats.implicits._
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.client.blaze._
import transport.train.TrainClient
import transport.subway.SubwayClient
import finance.{ IComptaClient, CMClient }
import db.DatabaseClient
import domoticz.DomoticzClient
import weather.WeatherClient

object DashboardServer extends IOApp {

  def run(args: List[String]) =
    ServerStream.stream[IO].compile.drain.as(ExitCode.Success)
}

object ServerStream {

  def trainService[F[_]: Effect](trainClient: TrainClient[F], settings: Settings) =
    new TrainService[F](trainClient, settings).service

  def subwayService[F[_]: Effect](subwayClient: SubwayClient[F], settings: Settings) =
    new SubwayService[F](subwayClient, settings).service

  def financeService[F[_]: Effect](icomptaClient: IComptaClient[F], cmClient: CMClient[F], settings: Settings) =
    new FinanceService[F](icomptaClient, cmClient, settings).service

  def energyService[F[_]: Effect](domoticzClient: DomoticzClient[F], settings: Settings) =
    new EnergyService[F](domoticzClient, settings).service

  def weatherService[F[_]: Effect](weatherClient: WeatherClient[F], settings: Settings) =
    new WeatherService[F](weatherClient, settings).service

  def stream[F[_]: ConcurrentEffect](implicit timer: Timer[F]) = {
    Settings.load() match {
      case Right(settings) =>
        for {
          httpClient <- Http1Client.stream[F]()
          trainClient <- TrainClient.stream[F](httpClient, settings)
          subwayClient = SubwayClient[F](trainClient)
          icomptaClient <- IComptaClient.stream[F](settings)
          cmClient <- CMClient.stream[F](httpClient, settings)
          domoticzClient = DomoticzClient[F](httpClient, settings.domoticz)
          weatherClient = WeatherClient[F](httpClient, settings.weather)
          R <- BlazeBuilder[F].bindHttp(settings.httpPort, "0.0.0.0")
                              .mountService(trainService(trainClient, settings), "/api/transport/train")
                              .mountService(subwayService(subwayClient, settings), "/api/transport/subway")
                              .mountService(financeService(icomptaClient, cmClient, settings), "/api/finance")
                              .mountService(energyService(domoticzClient, settings), "/api/energy")
                              .mountService(weatherService(weatherClient, settings), "/api/weather")
                              .serve
        } yield R

      case Left(error) =>
        sys.error(s"Malformed configuration file $error")
    }
  }
}
