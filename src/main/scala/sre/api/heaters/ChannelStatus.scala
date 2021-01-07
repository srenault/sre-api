package sre.api
package heaters

import scala.collection.SortedSet
import cats.effect._
import org.http4s.EntityEncoder
import org.http4s.circe._
import io.circe._
import io.circe.literal._

object ChannelStatus {

  def apply(id: Int, name: String, mode: String): ChannelStatus = {
    val m = Mode.getOrFail(mode.toInt)
    ChannelStatus(id, name, m)
  }

  implicit val encoder: Encoder[ChannelStatus] = new Encoder[ChannelStatus] {
    final def apply(channelStatus: ChannelStatus): Json = {
      json"""{ "id": ${channelStatus.id}, "name": ${channelStatus.name}, "mode": ${channelStatus.mode.id} }"""
    }
  }

  implicit def entitieEncoder[F[_]: Effect]: EntityEncoder[F, SortedSet[ChannelStatus]] = jsonEncoderOf[F, SortedSet[ChannelStatus]]

  implicit val ordering: Ordering[ChannelStatus] = Ordering.by(_.id)

  def readsSet(xml: scala.xml.Elem): SortedSet[ChannelStatus] = {
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

    SortedSet(fp0, fp1, fp2, fp3)
  }
}

case class ChannelStatus(id: Int, name: String, mode: Mode)
