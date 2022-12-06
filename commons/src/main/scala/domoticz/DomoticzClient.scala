package sre.api
package domoticz

import java.time.{LocalDate, Year, Month}
import java.time.format.DateTimeFormatter
import cats.effect._
import cats.effect.std.Dispatcher
import cats.implicits._
import cats.effect.unsafe.implicits._
import org.http4s.client._
import org.http4s.circe._
import io.circe._
import fs2.concurrent.Topic
import fs2.Stream
import sre.api.domoticz.websocket.{WebSocketClient, WebSocketListener}
import sre.api.domoticz.websocket.messages.WebSocketMessage

case class DomoticzClient[F[_]: Async](
  httpClient: Client[F],
  wsTopic: Topic[F, WebSocketMessage],
  settings: DomoticzSettings
)(implicit F: Concurrent[F]) extends DomoticzClientDsl[F] {

  lazy val wsConnect: Stream[F, Unit] = {
    val wsClient = DomoticzClient.createWsClient(wsTopic, settings)
    Stream.eval(wsClient.connect())
  }

  def graph[A : Decoder](
    sensor: Sensor,
    idx: Int,
    range: Range,
    maybeActYear: Option[Year] = None,
    maybeActMonth: Option[Month] = None
  ): F[List[A]] = {
    val uri = (settings.baseUri / "json.htm")
      .withQueryParam("type", "graph")
      .withQueryParam("sensor", sensor.value)
      .withQueryParam("idx", idx)
      .withQueryParam("range", range.value)

    val uri1 = maybeActYear.fold(uri) { year =>
      uri.withQueryParam("actyear", year.getValue)
    }

    val uri2 = maybeActMonth.fold(uri1) { month =>
      uri1.withQueryParam("actmonth", month.getValue)
    }

    val request = AuthenticatedGET(uri2)

    httpClient.expect[Json](request).map { response =>
      response.hcursor.downField("result").as[List[A]] match {
        case Left(e) => throw e
        case Right(result) => result
      }
    }
  }

  def switchLightCmd[A : Decoder](
    idx: Int,
    switchCmd: SwitchCmd
  ): F[A] = {
    val uri = (settings.baseUri / "json.htm")
      .withQueryParam("type", "command")
      .withQueryParam("param", "switchlight")
      .withQueryParam("idx", idx)
      .withQueryParam("switchcmd", switchCmd.value)
      .withQueryParam("level", "0")
      .withQueryParam("passcode", "")

    val request = AuthenticatedGET(uri)

    httpClient.expect[Json](request).map { response =>
      response.hcursor.downField("result").as[A] match {
        case Left(e) => throw e
        case Right(result) => result
      }
    }
  }
}

object DomoticzClient {

  private def createWsClient[F[_]: Concurrent](
    wsTopic: Topic[F, WebSocketMessage],
    settings: DomoticzSettings
  )(implicit F: Async[F]): WebSocketClient[F] = {
    val listener = {
      new WebSocketListener[F] {
        def onMessage(message: WebSocketMessage): F[Unit] = {
          wsTopic.publish1(message).map(_ => ())
        }
      }
    }

    WebSocketClient(settings)(listener)
  }

  def stream[F[_]: Async](httpClient: Client[F], settings: DomoticzSettings)(implicit F: Concurrent[F]): Stream[F, DomoticzClient[F]] = {
    Stream.eval {
      Topic[F, WebSocketMessage].flatMap { wsTopic =>
        F.pure(DomoticzClient(httpClient, wsTopic, settings))
      }
    }
  }
}

sealed trait SwitchCmd {
  def value: String
}

object SwitchCmd {
  case object Stop extends SwitchCmd {
    def value = "Stop"
  }

  case object Off extends Sensor {
    def value = "Off"
  }

  case object On extends Sensor {
    def value = "On"
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
