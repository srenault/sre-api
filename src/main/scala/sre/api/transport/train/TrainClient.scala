package sre.api.transport.train

import java.time.ZonedDateTime
import cats.effect._
import cats.implicits._
import io.circe._
import io.circe.literal._
import org.http4s._
import org.http4s.circe._
import org.http4s.client._
import sre.api.Settings

case class TrainClient[F[_]: Async](
    httpClient: Client[F],
    endpoint: Uri,
    authInfoRef: Ref[F, Option[Deferred[F, AuthResponse]]]
) extends TrainClientDsl[F] {

  def searchStations(term: String): F[List[Station]] = withAuthInfo {
    authInfo =>
      val uri =
        (endpoint / "autocomplete" / "stations").withQueryParam("q", term)
      val request = AuthenticatedGET(uri, authInfo)
      httpClient.expect[Json](request).map { response =>
        response.hcursor.downField("locations").as[List[Station]] match {
          case Left(e)         => throw e
          case Right(stations) => stations
        }
      }
  }

  def searchStation(term: String): F[Option[Station]] =
    searchStations(term).map(_.headOption)

  def nextDepartures(stationId: String): F[NextDepartures] = withAuthInfo {
    authInfo =>
      val uri = (endpoint / "station" / "trainBoards")
        .withQueryParam("stationUIC", stationId)
      val request = AuthenticatedGET(uri, authInfo)
      httpClient.expect[Json](request).map { response =>
        response.hcursor
          .downField("gl")
          .downField("arrivals")
          .as[NextDepartures] match {
          case Left(e)           => throw e
          case Right(departures) => departures
        }
      }
  }

  def nearestStops(
      latitude: Double,
      longitude: Double,
      distance: Int
  ): F[List[NearestStop]] =
    withAuthInfo { authInfo =>
      val uri = (endpoint / "aroundme" / "stops")
        .withQueryParam("latitude", latitude)
        .withQueryParam("longitude", longitude)
        .withQueryParam("maxDistance", distance)

      val request = AuthenticatedGET(uri, authInfo)
      httpClient.expect[Json](request).map { response =>
        response.hcursor
          .downField("stopLocations")
          .as[List[NearestStop]] match {
          case Left(e)             => throw e
          case Right(nearestStops) => nearestStops
        }
      }
    }

  def searchItineraries(
      stationA: Station,
      stationB: Station,
      date: ZonedDateTime
  ): F[List[Itinerary]] =
    withAuthInfo { authInfo =>
      val uri = (endpoint / "itinerary" / "search")
      val dateTimeFormatter =
        java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ssZ")
      val formattedDate = date.format(dateTimeFormatter)
      val body = json"""
        {
          "date": $formattedDate,
          "origin": $stationA,
          "destination": $stationB,
          "searchType": "DEPARTURE",
          "directItinerary": true,
          "exclusions": [],
          "filteredTransportTypes": [],
          "includedTransportTypes": [],
          "trainStationExclusion": null,
          "urbanStationExclusions": [],
          "via": null
        }"""

      val request = AuthenticatedPOST(uri, body, authInfo)
      httpClient.expect[Json](request).map { response =>
        response.hcursor.downField("itineraries").as[List[Itinerary]] match {
          case Left(e)            => throw e
          case Right(itineraries) => itineraries
        }
      }
    }
}

object TrainClient {

  def resource[F[_]: Async](
      httpClient: Client[F],
      settings: Settings
  ): Resource[F, TrainClient[F]] =
    Resource.eval {
      (for {
        d <- Deferred[F, AuthResponse]
        authInfoRef <- Ref.of[F, Option[Deferred[F, AuthResponse]]](None)
      } yield {
        TrainClient[F](
          httpClient,
          settings.transport.train.endpoint,
          authInfoRef
        )
      })
    }
}
