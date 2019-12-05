package sre.api.finance.cm

import java.time.LocalDate
import cats.effect._
import org.http4s._
import org.http4s.EntityEncoder
import org.http4s.circe._
import io.circe.Encoder
import io.circe.generic.semiauto._
import sre.api.CMAccountSettings

case class CMAccountState(
  id: String,
  `type`: CMAccountType,
  label: String,
  displayName: Option[String],
  statements: List[CMStatement]
) {
  def since(date: LocalDate): CMAccountState = {
    this.copy(statements = statements.filter { statement =>
      statement.date.equals(date) || statement.date.isAfter(date)
    })
  }

  def toOverview: CMAccountOverview = {
    val balance = statements.lastOption.flatMap(_.balance).getOrElse {
      sys.error(s"Unable to get balance for account ${id}")
    }

    CMAccountOverview(
      id,
      `type`,
      label,
      displayName,
      balance
    )
  }
}

object CMAccountState {

  def apply(accountSettings: CMAccountSettings, statements: List[CMStatement]): CMAccountState = {
    CMAccountState(
      id = accountSettings.id,
      `type` = accountSettings.`type`,
      label = accountSettings.label,
      displayName = None,
      statements = statements
    )
  }

  def unknown(id: String, label: String, statements: List[CMStatement]): CMAccountState = {
    CMAccountState(id, CMAccountType.Unknown, label, displayName = None, statements)
  }

  implicit val encoder: Encoder[CMAccountState] =
    deriveEncoder[CMAccountState]

  implicit def entityEncoder[F[_]: Effect]: EntityEncoder[F, CMAccountState] =
    jsonEncoderOf[F, CMAccountState]

  implicit def entitiesEncoder[F[_]: Effect]: EntityEncoder[F, List[CMAccountState]] =
    jsonEncoderOf[F, List[CMAccountState]]
}
