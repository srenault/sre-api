package sre.api.finance.cm

import io.circe._

sealed trait CMAccountType {
  def id: String
}

object CMAccountType {

  def apply(id: String): CMAccountType = {
    id match {
      case id if id == Saving.id => Saving
      case id if id == Current.id => Current
      case id if id == Joint.id => Joint
      case _ => Unknown
    }
  }

  implicit val encoder: Encoder[CMAccountType] = new Encoder[CMAccountType] {
    final def apply(a: CMAccountType): Json = Json.fromString(a.id)
  }

  implicit val decoder: Decoder[CMAccountType] = new Decoder[CMAccountType] {
    final def apply(c: HCursor): Decoder.Result[CMAccountType] =
      c.as[String].map { id =>
        CMAccountType(id)
      }
  }

  case object Saving extends CMAccountType {
    def id = "saving_account"
  }

  case object Current extends CMAccountType {
    def id = "current_account"
  }

  case object Joint extends CMAccountType {
    def id = "joint_account"
  }

  case object Unknown extends CMAccountType {
    def id = "n/a"
  }
}
