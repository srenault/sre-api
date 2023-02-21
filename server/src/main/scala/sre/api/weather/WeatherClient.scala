package sre.api.weather

import cats.effect._
import cats.implicits._
import io.circe._
import org.http4s.Uri
import org.http4s.circe._
import org.http4s.client._
import sre.api.WeatherSettings

case class WeatherClient[F[_]: Concurrent](
    httpClient: Client[F],
    settings: WeatherSettings
) extends WeatherClientDsl[F] {

  def searchStation(term: String): F[List[MatchedStation]] = {
    val path = Uri.Path.unsafeFromString("station-meteo/search")
    val uri = settings.endpoint.withQueryParam("search", term)
    val request = AuthenticatedGET(uri, path)

    httpClient.expect[Json](request).map { response =>
      response.hcursor
        .downField("DATA")
        .downField("matches")
        .as[List[MatchedStation]] match {
        case Left(e)       => throw e
        case Right(result) => result
      }
    }
  }

  def getStation(id: String): F[Station] = {
    val path = Uri.Path.unsafeFromString("station-meteo")
    val uri = settings.endpoint.withQueryParam("id", id)
    val request = AuthenticatedGET(uri, path)

    httpClient.expect[Json](request).map { response =>
      response.hcursor.downField("DATA").as[Station] match {
        case Left(e)       => throw e
        case Right(result) => result
      }
    }
  }

  def searchCity(
      term: String,
      latitude: Double,
      longitude: Double
  ): F[List[City]] = {
    val path = Uri.Path.unsafeFromString("geolocalisation")
    val uri = settings.endpoint
      .withQueryParam("prev_box", "1")
      .withQueryParam("search", term)
      .withQueryParam("latitude", latitude)
      .withQueryParam("longitude", longitude)
    val request = AuthenticatedGET(uri, path)

    httpClient.expect[Json](request).map { response =>
      response.hcursor.downField("DATA").as[List[City]] match {
        case Left(e)       => throw e
        case Right(result) => result
      }
    }
  }

  def getForecast(geoId: Long): F[Forecast] = {
    val path = Uri.Path.unsafeFromString("prevision-automatique")
    val uri = settings.endpoint
      .withQueryParam("pays", "FR")
      .withQueryParam("geoid", geoId)
    val request = AuthenticatedGET(uri, path)

    httpClient.expect[Json](request).map { response =>
      response.hcursor.downField("DATA").as[Forecast] match {
        case Left(e)       => throw e
        case Right(result) => result
      }
    }
  }
}
