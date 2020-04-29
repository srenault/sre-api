package sre.api

import cats.effect._
import cats.implicits._
import org.http4s._
import weather.WeatherClient

class WeatherService[F[_]: Effect](weatherClient: WeatherClient[F], settings: Settings) extends WeatherServiceDsl[F] {

  val service: HttpRoutes[F] = {
    HttpRoutes.of[F] {
      case GET -> Root / "stations" / "search" / term =>
        weatherClient.searchStation(term).flatMap { result =>
          Ok(result)
        }

      case GET -> Root / "stations" / stationId =>
        weatherClient.getStation(stationId).flatMap { result =>
          Ok(result)
        }

      case GET -> Root / "city" / "search" / term :? LatitudeQueryParamMatcher(latitude) +& LongitudeQueryParamMatcher(longitude) =>
        weatherClient.searchCity(term, latitude, longitude).flatMap { result =>
          Ok(result)
        }

      case GET -> Root / "city" / geoId / "forecast" =>
        weatherClient.getForecast(geoId.toLong).flatMap { result =>
          Ok(result)
        }
    }
  }
}
