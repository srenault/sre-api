package sre.api.finance.cm

import java.time.LocalDate
import cats.effect._
import org.http4s._
import org.http4s.EntityEncoder
import org.http4s.circe._
import io.circe.Encoder
import io.circe.generic.semiauto._

case class CMAccount(
  id: String,
  `type`: CMAccountType,
  label: String,
  displayName: Option[String],
  balance: Float,
  statements: List[CMStatement]
) {
  def since(date: LocalDate): CMAccount = {
    this.copy(statements = statements.filter { statement =>
      statement.date.equals(date) || statement.date.isAfter(date)
    })
  }
}

object CMAccount {
  implicit val encoder: Encoder[CMAccount] = deriveEncoder[CMAccount]
  implicit def entityEncoder[F[_]: Effect]: EntityEncoder[F, CMAccount] = jsonEncoderOf[F, CMAccount]
  implicit def entitiesEncoder[F[_]: Effect]: EntityEncoder[F, List[CMAccount]] = jsonEncoderOf[F, List[CMAccount]]
}
