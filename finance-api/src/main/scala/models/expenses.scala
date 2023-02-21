package sre.api.finance

import cats.effect._
import org.http4s.EntityEncoder
import org.http4s.circe._
import io.circe.Encoder
import io.circe.generic.semiauto._

case class ExpensesByCategory(
    id: String,
    label: String,
    amount: Float,
    threshold: Int
)

object ExpensesByCategory {
  implicit val encoder: Encoder[ExpensesByCategory] =
    deriveEncoder[ExpensesByCategory]
  implicit def entityEncoder[F[_]: Sync]: EntityEncoder[F, ExpensesByCategory] =
    jsonEncoderOf[ExpensesByCategory]
  implicit def entitiesEncoder[F[_]: Sync]
      : EntityEncoder[F, List[ExpensesByCategory]] =
    jsonEncoderOf[List[ExpensesByCategory]]
}
