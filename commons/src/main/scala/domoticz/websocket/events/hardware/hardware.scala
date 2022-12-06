package sre.api
package domoticz
package websocket.events.hardware

import io.circe._
import java.time.LocalDateTime

trait HardwareEvent

object HardwareEvent {

  case class Data(
    name: String,
    value: String,
    lastUpdate: LocalDateTime
  ) extends HardwareEvent

  object Data {

    val localDateTimeDecoder = new Decoder[LocalDateTime] {
      final def apply(c: HCursor): Decoder.Result[LocalDateTime] = {
        val dateTimeFormatter = java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
        c.as[String].map(LocalDateTime.parse(_, dateTimeFormatter))
      }
    }

    implicit val decoder: Decoder[Data] = new Decoder[Data] {
      final def apply(c: HCursor): Decoder.Result[Data] =
        for {
          value <- c.downField("Data").as[String]
          name <- c.downField("Name").as[String]
          lastUpdate <- c.downField("LastUpdate").as[LocalDateTime](localDateTimeDecoder)
        } yield Data(name, value, lastUpdate)
    }
  }

  case class Unknown(data: Json) extends HardwareEvent
  object Unknown {
    implicit val decoder: Decoder[Unknown] = new Decoder[Unknown] {
      final def apply(c: HCursor): Decoder.Result[Unknown] = {
        c.as[Json].map(Unknown(_))
      }
    }

    implicit val encoder: Encoder[Unknown] = new Encoder[Unknown] {
      final def apply(unknown: Unknown): Json = {
        unknown.data
      }
    }
  }


  implicit val decoder: Decoder[HardwareEvent] = new Decoder[HardwareEvent] {

    final def apply(c: HCursor): Decoder.Result[HardwareEvent] =
      c.downField("HardwareName").as[String].flatMap {
        case hardwareName if hardwareName == Teleinfo.HARDWARE_NAME =>
          c.as[Teleinfo]

        case _ =>
          c.as[Unknown]
      }
  }
}
