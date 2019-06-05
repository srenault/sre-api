package sre.api

import cats.effect._
import cats.implicits._
import org.http4s.HttpService

import transport.subway._

class SubwayService[F[_]: Effect](subwayClient: SubwayClient[F], settings: Settings) extends TrainServiceDsl[F] {
  val service: HttpService[F] = {
    HttpService[F] {
      case GET -> Root / "stops" / stopId / "departures" =>
        subwayClient.nextDepartures(stopId).flatMap { nextDepartures =>
          Ok(nextDepartures)
        }
    }
  }
}
