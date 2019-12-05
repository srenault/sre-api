package sre.api.finance.cm

import cats.effect._
import org.http4s._
import org.http4s.EntityEncoder
import org.http4s.circe._
import io.circe.Encoder
import io.circe.generic.semiauto._

case class CMAccountOverview(
  id: String,
  `type`: CMAccountType,
  label: String,
  displayName: Option[String],
  balance: Float
)

object CMAccountOverview {

  implicit val encoder: Encoder[CMAccountOverview] =
    deriveEncoder[CMAccountOverview]

  implicit def entityEncoder[F[_]: Effect]: EntityEncoder[F, CMAccountOverview] =
    jsonEncoderOf[F, CMAccountOverview]

  implicit def entitiesEncoder[F[_]: Effect]: EntityEncoder[F, List[CMAccountOverview]] =
    jsonEncoderOf[F, List[CMAccountOverview]]
}
