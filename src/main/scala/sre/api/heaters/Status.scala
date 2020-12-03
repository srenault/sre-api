package sre.api
package heaters

import cats.effect._
import org.http4s.EntityEncoder
import org.http4s.circe._
import io.circe._
import io.circe.literal._
import io.circe.generic.semiauto._

sealed trait Mode {
  def id: Int
  def name: String
}

object Mode {

  def get(id: Int): Option[Mode] =
    if (Confort.id == id) {
      Some(Confort)
    } else if (ConfortMoins1.id == id) {
      Some(ConfortMoins1)
    } else if (ConfortMoins2.id == id) {
      Some(ConfortMoins2)
    } else if (Eco.id == id) {
      Some(Eco)
    } else if (HorsGel.id == id) {
      Some(HorsGel)
    } else if (Arret.id == id) {
      Some(Arret)
    } else None

  def getOrFail(id: Int): Mode =
    get(id) getOrElse {
      throw new RuntimeException(s"Unknown mode: $id")
    }

  def validate(id: Int): Either[String, Mode] = {
    get(id) match {
      case Some(mode) => Right(mode)
      case None => Left(s"Unknown mode $id")
    }
  }

  case object ConfortMoins2 extends Mode {
    val id = 5
    val name = "Confort -2"
  }

  case object ConfortMoins1 extends Mode {
    val id = 4
    val name = "Confort -1"
  }

  case object Confort extends Mode {
    val id = 3
    val name = "Confort"
  }

  case object Eco extends Mode {
    val id = 1
    val name = "Eco"
  }

  case object HorsGel extends Mode {
    val id = 2
    val name = "Hors gel"
  }

  case object Arret extends Mode {
    val id = 0
    val name = "Arret"
  }

  implicit val encoder: Encoder[Mode] = new Encoder[Mode] {
    final def apply(mode: Mode): Json = {
      json"""{ "id": ${mode.id}, "name": ${mode.name} }"""
    }
  }
}

case class ChannelsStatus(
  fp0: ChannelStatus,
  fp1: ChannelStatus,
  fp2: ChannelStatus,
  fp3: ChannelStatus
)

object ChannelsStatus {

  implicit val encoder: Encoder[ChannelsStatus] = deriveEncoder[ChannelsStatus]
  implicit def entitieEncoder[F[_]: Effect]: EntityEncoder[F, ChannelsStatus] = jsonEncoderOf[F, ChannelsStatus]

  def reads(xml: scala.xml.Elem): ChannelsStatus = {
    val out = (xml \ "out")
    val outFP0 = (out \ "FP0").text
    val outFP1 = (out \ "FP1").text
    val outFP2 = (out \ "FP2").text
    val outFP3 = (out \ "FP3").text

    val name = (xml \ "name")
    val nameFP0 = (name \ "FP0").text
    val nameFP1 = (name \ "FP1").text
    val nameFP2 = (name \ "FP2").text
    val nameFP3 = (name \ "FP3").text

    val fp0 = ChannelStatus(0, nameFP0, outFP0)
    val fp1 = ChannelStatus(1, nameFP1, outFP1)
    val fp2 = ChannelStatus(2, nameFP2, outFP2)
    val fp3 = ChannelStatus(3, nameFP3, outFP3)

    ChannelsStatus(fp0, fp1, fp2, fp3)
  }
}

case class ChannelStatus(id: Int, name: String, mode: Mode)

object ChannelStatus {

  def apply(id: Int, name: String, mode: String): ChannelStatus = {
    val m = Mode.getOrFail(mode.toInt)
    ChannelStatus(id, name, m)
  }

  implicit val encoder: Encoder[ChannelStatus] = deriveEncoder[ChannelStatus]
}
