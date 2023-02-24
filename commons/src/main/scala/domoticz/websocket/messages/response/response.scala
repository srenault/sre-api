package sre.api
package domoticz
package websocket
package messages

import io.circe._
import io.circe.parser._
import events.hardware.HardwareEvent

// -- Response
sealed trait Response extends WebSocketMessage

object Response {
  val MESSAGE_TYPE = "response"

  case class Device(events: List[HardwareEvent]) extends Response
  object Device {
    val REQUEST_TYPE = "device_request"

    implicit val decoder: Decoder[Device] = new Decoder[Device] {
      final def apply(c: HCursor): Decoder.Result[Device] = {
        for {
          data <- c.downField("data").as[String]
          dataJson <- parse(data).left.map(error =>
            DecodingFailure(error.message, c.history)
          )
          events <- dataJson.hcursor.downField("result").as[List[HardwareEvent]]
        } yield Device(events)
      }
    }
  }

  case class Unknown(message: Json) extends Response
  object Unknown {
    implicit val decoder: Decoder[Unknown] = new Decoder[Unknown] {
      final def apply(c: HCursor): Decoder.Result[Unknown] = {
        c.as[Json].map(Unknown(_))
      }
    }
  }

  implicit val decoder: Decoder[Response] = new Decoder[Response] {
    final def apply(c: HCursor): Decoder.Result[Response] = {
      c.downField("request").as[String].flatMap {
        case requestType if requestType == Device.REQUEST_TYPE =>
          c.as[Device]

        case _ =>
          c.as[Unknown]
      }
    }
  }
}
