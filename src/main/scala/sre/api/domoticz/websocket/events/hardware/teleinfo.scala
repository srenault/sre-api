package sre.api
package domoticz
package websocket.events.hardware

import io.circe._
import io.circe.syntax._
import io.circe.generic.semiauto._
import java.time.LocalDateTime
import scala.util.{Try, Failure, Success}

// - Teleinfo
sealed trait Teleinfo extends HardwareEvent {

  def `type`: String

  def isUnknown: Boolean =
    this match {
      case _:Teleinfo.Unknown => true
      case _ => false
    }
}

object Teleinfo {

  val HARDWARE_NAME = "Teleinfo"

  case class Unknown(`type`: String, data: Json) extends Teleinfo

  object Unknown {
    val TYPE = "teleinfo_unknown"

    implicit val decoder: Decoder[Unknown] = new Decoder[Unknown] {
      final def apply(c: HCursor): Decoder.Result[Unknown] = {
        c.as[Json].map(Unknown(TYPE, _))
      }
    }

    implicit val encoder: Encoder[Unknown] = deriveEncoder[Unknown]
  }

  case class Current(
    `type`: String,
    name: String,
    value: Float,
    lastUpdate: LocalDateTime
  ) extends Teleinfo

  object Current {

    val TYPE = "teleinfo_current"

    val Reg = """^([\d]+(\.[\d]+)?) A$""".r

    val NAME = "Teleinfo Courant"

    implicit val encoder: Encoder[Current] = deriveEncoder[Current]

    implicit val decoder: Decoder[Current] = new Decoder[Current] {

      final def apply(c: HCursor): Decoder.Result[Current] = {
        c.as[HardwareEvent.Data].flatMap { event =>
          event.value match {
            case Reg(valueAsText, _) =>
              Try(valueAsText.toFloat) match {
                case Success(value) =>
                  Right(Current(TYPE, event.name, value, event.lastUpdate))

                case Failure(error) =>
                  Left(DecodingFailure(error.getMessage, c.history))
              }
          }
        }
      }
    }
  }

  case class Load(
    `type`: String,
    name: String,
    value: Float,
    lastUpdate: LocalDateTime
  ) extends Teleinfo

  object Load {

    val TYPE = "teleinfo_load"

    val Reg = """^([\d]+(\.[\d]+)?)%$""".r

    val NAME = "Teleinfo Pourcentage de Charge"

    implicit val encoder: Encoder[Load] = deriveEncoder[Load]

    implicit val decoder: Decoder[Load] = new Decoder[Load] {

      final def apply(c: HCursor): Decoder.Result[Load] =
        c.as[HardwareEvent.Data].flatMap { event =>
          event.value match {
            case Reg(valueAsText, _) =>
              Try(valueAsText.toFloat) match {
                case Success(value) =>
                  Right(Load(TYPE, event.name, value, event.lastUpdate))

                case Failure(error) =>
                  Left(DecodingFailure(error.getMessage, c.history))
              }
          }
        }
    }
  }

  implicit val encoder: Encoder[Teleinfo] = new Encoder[Teleinfo] {
    final def apply(teleinfo: Teleinfo): Json = {
      teleinfo match {
        case l: Load => l.asJson
        case c: Current => c.asJson
        case u: Unknown => u.asJson
      }
    }
  }

  implicit val decoder: Decoder[Teleinfo] = new Decoder[Teleinfo] {

    final def apply(c: HCursor): Decoder.Result[Teleinfo] =
      c.downField("Name").as[String].flatMap {
        case name if name == Load.NAME =>
          c.as[Load]

        case name if name == Current.NAME =>
          c.as[Current]

        case _ =>
          c.as[Unknown]
      }
  }
}
