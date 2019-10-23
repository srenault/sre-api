package sre.api
package finance
package cm

import java.time.LocalDate
import cats.effect._
import org.http4s._
import org.http4s.EntityEncoder
import org.http4s.circe._
import io.circe.Encoder
import io.circe.generic.semiauto._

case class CMStatement(
  fitid: String,
  accountId: String,
  date: LocalDate,
  amount: Float,
  label: String,
  balance: Option[Float]
) {
  def id: String = List(fitid, accountId, date, amount, label).mkString("#")
}

object CMStatement {
  implicit val encoder: Encoder[CMStatement] = deriveEncoder[CMStatement]
  implicit def entityEncoder[F[_]: Effect]: EntityEncoder[F, CMStatement] = jsonEncoderOf[F, CMStatement]
  implicit def entitiesEncoder[F[_]: Effect]: EntityEncoder[F, List[CMStatement]] = jsonEncoderOf[F, List[CMStatement]]
}
