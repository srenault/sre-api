package sre.api.finance.cm

import io.circe.{ Json, Encoder }

sealed trait CMAccountType {
  def id: String
}

object CMAccountType {

  implicit val encoder: Encoder[CMAccountType] = new Encoder[CMAccountType] {
    final def apply(a: CMAccountType): Json = Json.fromString(a.id)
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
