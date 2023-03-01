package sre.api.shutters

import io.circe._
import io.circe.syntax._

sealed trait State {
  def id: String
}

object State {

  def get(id: String): Option[State] =
    if (Opened.id == id) {
      Some(Opened)
    } else if (Closed.id == id) {
      Some(Closed)
    } else if (Stopped.id == id) {
      Some(Stopped)
    } else None

  def getOrFail(id: String): State =
    get(id) getOrElse {
      throw new RuntimeException(s"Unknown state: $id")
    }

  def validate(id: String): Either[String, State] = {
    get(id) match {
      case Some(state) => Right(state)
      case None        => Left(s"Unknown state $id")
    }
  }

  case object Opened extends State {
    val id = "opened"
  }

  case object Closed extends State {
    val id = "closed"
  }

  case object Stopped extends State {
    val id = "stopped"
  }

  val all: Set[State] =
    Set(Opened, Closed, Stopped)

  implicit val encoder: Encoder[State] = new Encoder[State] {
    final def apply(state: State): Json = {
      state.id.asJson
    }
  }
}
