package sre.api.domoticz

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import cats.effect._
import cats.implicits._
import org.http4s.client._
import org.http4s.circe._
import io.circe._
import sre.api.DomoticzSettings

case class DomoticzClient[F[_]: ConcurrentEffect](httpClient: Client[F], settings: DomoticzSettings) extends DomoticzClientDsl[F] {

  def graph[A : Decoder](sensor: Sensor, idx: Int, range: Range): F[List[A]] = {
    val uri = (settings.endpoint / "json.htm")
      .withQueryParam("type", "graph")
      .withQueryParam("sensor", sensor.value)
      .withQueryParam("idx", idx)
      .withQueryParam("range", range.value)

    val request = AuthenticatedGET(uri)

    httpClient.expect[Json](request).map { response =>
      response.hcursor.downField("result").as[List[A]] match {
        case Left(e) => throw e
        case Right(result) => result
      }
    }
  }
}

sealed trait Sensor {
  def value: String
}

object Sensor {
  case object Counter extends Sensor {
    def value = "counter"
  }

  case object Percentage extends Sensor {
    def value = "Percentage"
  }
}

sealed trait Range {
  def value: String
}

object Range {
  case object Day extends Range {
    def value = "day"
  }

  case object Week extends Range {
    def value = "week"
  }

  case object Month extends Range {
    def value = "month"
  }

  case object Year extends Range {
    def value = "year"
  }

  case class Period(start: LocalDate, end: LocalDate) extends Range {
    def value = {
      val s = start.format(DateTimeFormatter.ISO_LOCAL_DATE)
      val e = end.format(DateTimeFormatter.ISO_LOCAL_DATE)
      s"${s}T${e}"
    }
  }
}
