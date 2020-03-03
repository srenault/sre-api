package sre.api
package domoticz

import org.slf4j.LoggerFactory
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import cats.effect._
import cats.implicits._
import org.http4s.client._
import org.http4s.circe._
import io.circe._
import io.circe.parser._
import fs2.concurrent.{SignallingRef, Topic}
import fs2.Stream
import sre.api.DomoticzSettings
import websocket.{WebSocketClient, WebSocketListener}
import websocket.messages.WebSocketMessage

case class DomoticzClient[F[_]](
  httpClient: Client[F],
  wsTopic: Topic[F, WebSocketMessage],
  wsInterrupter: SignallingRef[F, Boolean],
  settings: DomoticzSettings
)(implicit F: ConcurrentEffect[F]) extends DomoticzClientDsl[F] {

  val logger = LoggerFactory.getLogger("sre.api.domoticz.DomoticzClient")

  private lazy val wsClient: WebSocketClient[F] =
    WebSocketClient(settings)(new WebSocketListener {
      def onOpen(): Unit = {
        println("open")
      }

      def onMessage(message: String): Unit = {
        parse(message).flatMap(_.as[WebSocketMessage]) match {
          case Right(message) =>
            //val events = Stream.emits(message.events).compile.drain
            //F.runAsync(wsTopic.publish(events))(_ => IO.unit).unsafeRunSync
            F.runAsync(wsTopic.publish1(message))(_ => IO.unit).unsafeRunSync

          case Left(error) =>
            logger.warn(s"Unable to parse domoticz event from:\n${message}\n${error}")
        }
      }

      def onClose(code: Int, reason: String, remote: Boolean): Unit = {
        println("close")
      }

      def onError(ex: Exception): Unit = {
        println("error")
      }
    })

  def initWebSocket(): Stream[F, Unit] = {
    Stream.eval(wsClient.connect())
  }

  def graph[A : Decoder](sensor: Sensor, idx: Int, range: Range): F[List[A]] = {
    val uri = (settings.baseUri / "json.htm")

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

object DomoticzClient {

  def stream[F[_]: ConcurrentEffect](httpClient: Client[F], settings: DomoticzSettings): Stream[F, DomoticzClient[F]] = {
    val client = for {
      wsInterrupter <- SignallingRef[F, Boolean](false)
      wsTopic <- Topic[F, WebSocketMessage](WebSocketMessage.Init)
    } yield {
      DomoticzClient(httpClient, wsTopic, wsInterrupter, settings)
    }

    Stream.eval(client)
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
