package sre.dashboard.finance

import cats.effect._
import org.http4s.EntityEncoder
import org.http4s.circe._
import io.circe.Encoder
import io.circe.generic.semiauto._

case class ExpensesByCategory(id: String, label: String, amount: Float, threshold: Int)

object ExpensesByCategory {
  implicit val encoder: Encoder[ExpensesByCategory] = deriveEncoder[ExpensesByCategory]
  implicit def entityEncoder[F[_]: Effect]: EntityEncoder[F, ExpensesByCategory] = jsonEncoderOf[F, ExpensesByCategory]
  implicit def entitiesEncoder[F[_]: Effect]: EntityEncoder[F, List[ExpensesByCategory]] = jsonEncoderOf[F, List[ExpensesByCategory]]
}
