package sre.api
package finance
package cm

import java.time.LocalDate
import cats.effect._
import org.http4s._
import org.http4s.EntityEncoder
import org.http4s.circe._
import io.circe._
import io.circe.literal._
import io.circe.generic.semiauto._

case class CMStatement(
  fitid: String,
  accountId: String,
  date: LocalDate,
  amount: Float,
  label: String,
  balance: Option[Float]
) {
  def id: String = {
    val v = List(fitid, accountId, date, amount, label).mkString("#")
    java.util.Base64.getEncoder().encodeToString(v.getBytes("UTF-8"))
  }
}

object CMStatement {
  implicit def entityEncoder[F[_]: Effect]: EntityEncoder[F, CMStatement] = jsonEncoderOf[F, CMStatement]
  implicit val encoder: Encoder[CMStatement] = new Encoder[CMStatement] {
    final def apply(statement: CMStatement): Json = {
      json"""
       {
         "id": ${statement.id},
         "fitid": ${statement.fitid},
         "accountId": ${statement.accountId},
         "date": ${statement.date},
         "amount": ${statement.amount},
         "label": ${statement.label},
         "balance": ${statement.balance}
       }
      """
    }
  }
  implicit def entitiesEncoder[F[_]: Effect]: EntityEncoder[F, List[CMStatement]] = jsonEncoderOf[F, List[CMStatement]]
}
