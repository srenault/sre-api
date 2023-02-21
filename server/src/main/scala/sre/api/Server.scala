package sre.api

import cats.Parallel
import cats.effect._
import com.comcast.ip4s._
import org.http4s.ember.server._
import org.http4s.ember.client._
import org.http4s.server.Router
import org.http4s.implicits._
import org.http4s.client.middleware.{RequestLogger, ResponseLogger}
import transport.train.TrainClient
import transport.subway.SubwayClient
import domoticz.DomoticzClient
import weather.WeatherClient
import energy.EnergyClient

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    Server.resource[IO].use(_ => IO.never).as(ExitCode.Success)
}

object Server {

  def trainService[F[_]: Async](
      trainClient: TrainClient[F],
      settings: Settings
  ) =
    new TrainService[F](trainClient, settings).service

  def subwayService[F[_]: Async](
      subwayClient: SubwayClient[F],
      settings: Settings
  ) =
    new SubwayService[F](subwayClient, settings).service

  def energyService[F[_]: Async](
      energyClient: EnergyClient[F],
      settings: Settings
  ) =
    new EnergyService[F](energyClient, settings).service

  def weatherService[F[_]: Async](
      weatherClient: WeatherClient[F],
      settings: Settings
  ) =
    new WeatherService[F](weatherClient, settings).service

  def resource[F[_]: Concurrent: Parallel](implicit F: Async[F]) = {
    Settings.load() match {
      case Right(settings) =>
        for {
          httpClient <- {
            EmberClientBuilder.default[F].build.map { client =>
              val updatedClient = if (settings.httpClient.logRequest) {
                RequestLogger(logHeaders = true, logBody = true)(client)
              } else client

              if (settings.httpClient.logResponse) {
                ResponseLogger(logHeaders = true, logBody = true)(updatedClient)
              } else updatedClient
            }
          }

          trainClient <- TrainClient.resource[F](httpClient, settings)

          subwayClient = SubwayClient[F](trainClient)

          domoticzClient <- DomoticzClient
            .resource[F](httpClient, settings.domoticz)

          _ <- domoticzClient.wsConnect

          energyClient = EnergyClient[F](domoticzClient, settings)

          weatherClient = WeatherClient[F](httpClient, settings.weather)

          httpApp = Router(
            "/api/transport/train" -> trainService(trainClient, settings),
            "/api/transport/subway" -> subwayService(subwayClient, settings),
            "/api/energy" -> energyService(energyClient, settings),
            "/api/weather" -> weatherService(weatherClient, settings)
          ).orNotFound

          R <- EmberServerBuilder
            .default[F]
            .withHost(ipv4"0.0.0.0")
            .withPort(Port.fromInt(settings.httpPort).get)
            .withHttpApp(httpApp)
            .build
        } yield R

      case Left(error) =>
        sys.error(s"Malformed configuration file $error")
    }
  }
}
