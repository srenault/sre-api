package sre.api
package domoticz
package websocket
package messages

import io.circe._

trait WebSocketMessage

object WebSocketMessage {
  case object Init extends WebSocketMessage

  // -- WebSocket

  case class Unknown(message: Json) extends WebSocketMessage
  object Unknown {
    implicit val decoder: Decoder[Unknown] = new Decoder[Unknown] {
      final def apply(c: HCursor): Decoder.Result[Unknown] = {
        c.as[Json].map(Unknown(_))
      }
    }
  }

  implicit val decoder: Decoder[WebSocketMessage] =
    new Decoder[WebSocketMessage] {
      final def apply(c: HCursor): Decoder.Result[WebSocketMessage] = {
        c.downField("event").as[String].flatMap {
          case eventType if eventType == Response.MESSAGE_TYPE =>
            c.as[Response]

          case _ =>
            c.as[Unknown]
        }
      }
    }
}
