package sre.api.transport.subway

import cats.effect._
import cats.implicits._
import io.circe._
import io.circe.literal._
import org.http4s.circe._
import sre.api.transport.train._

case class SubwayClient[F[_]: ConcurrentEffect](trainClient: TrainClient[F]) {

  def nextDepartures(stopId: String): F[List[NextDeparture]] = trainClient.withAuthInfo { authInfo =>
    val uri = (trainClient.endpoint / "ratp" / "nextDepartures").withQueryParam("stopId", stopId)
    val request = trainClient.AuthenticatedGET(uri, authInfo)
    trainClient.httpClient.expect[Json](request).map { response =>
      response.hcursor.downField("nextDepartures").as[List[NextDeparture]] match {
        case Left(e) => throw e
        case Right(departures) => departures
      }
    }
  }
}
