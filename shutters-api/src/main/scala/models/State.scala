package sre.api.shutters

import io.circe._
import io.circe.syntax._

sealed trait Action {
  def id: String
}

object Action {

  def get(id: String): Option[Action] =
    if (Open.id == id) {
      Some(Open)
    } else if (Close.id == id) {
      Some(Close)
    } else if (Stop.id == id) {
      Some(Stop)
    } else None

  def getOrFail(id: String): Action =
    get(id) getOrElse {
      throw new RuntimeException(s"Unknown action: $id")
    }

  def validate(id: String): Either[String, Action] = {
    get(id) match {
      case Some(action) => Right(action)
      case None         => Left(s"Unknown action $id")
    }
  }

  case object Open extends Action {
    val id = "open"
  }

  case object Close extends Action {
    val id = "close"
  }

  case object Stop extends Action {
    val id = "stop"
  }

  val all: Set[Action] =
    Set(Open, Close, Stop)

  implicit val encoder: Encoder[Action] = new Encoder[Action] {
    final def apply(action: Action): Json = {
      action.id.asJson
    }
  }
}
