package sre.api

import cats.effect._
import cats.implicits._
import org.http4s._
import java.time.ZonedDateTime
import transport.train._

class TrainService[F[_]: Async](trainClient: TrainClient[F], settings: Settings)
    extends TrainServiceDsl[F] {
  val service: HttpRoutes[F] = {
    HttpRoutes.of[F] {
      case GET -> Root / "authenticate" =>
        trainClient.authenticate().flatMap { authResponse =>
          Ok(authResponse)
        }

      case GET -> Root / "stations" / "search" / term =>
        trainClient.searchStations(term).flatMap { stations =>
          Ok(stations)
        }

      case GET -> Root / "stations" / stationId / "departures" =>
        trainClient.nextDepartures(stationId).flatMap { nextDepartures =>
          Ok(nextDepartures)
        }

      case GET -> Root / "itineraries" / "search" / departure / arrival :? DateQueryParamMatcher(
            maybeDate
          ) =>
        val searchStationA = trainClient.searchStation(departure)
        val searchStationB = trainClient.searchStation(arrival)

        (searchStationA, searchStationB).tupled.flatMap {
          case (Some(stationA), Some(stationB)) =>
            val date = maybeDate getOrElse ZonedDateTime.now()
            trainClient
              .searchItineraries(stationA, stationB, date)
              .flatMap(Ok(_))

          case _ => BadRequest()
        }

      case GET -> Root / "near" / "stops" :? LatitudeQueryParamMatcher(
            latitude
          ) +& LongitudeQueryParamMatcher(
            longitude
          ) +& DistanceQueryParamMatcher(distance) =>
        trainClient.nearestStops(latitude, longitude, distance).flatMap {
          nearestStops =>
            Ok(nearestStops)
        }
    }
  }
}
