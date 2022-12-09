package sre.api.heaters

import scala.collection.SortedSet
import io.circe._
import io.circe.literal._

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
      case None       => Left(s"Unknown mode $id")
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

  implicit val ordering: Ordering[Mode] = Ordering.by(_.id)

  val all: SortedSet[Mode] =
    SortedSet(Arret, HorsGel, Eco, Confort, ConfortMoins1, ConfortMoins2)

  implicit val encoder: Encoder[Mode] = new Encoder[Mode] {
    final def apply(mode: Mode): Json = {
      json"""{ "id": ${mode.id}, "name": ${mode.name} }"""
    }
  }
}
