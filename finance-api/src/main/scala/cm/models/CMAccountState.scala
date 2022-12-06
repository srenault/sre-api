package sre.api.finance
package cm

import cats.effect._
import org.http4s.EntityEncoder
import org.http4s.circe._
import io.circe.Encoder
import io.circe.generic.semiauto._
import analytics.Period
import sre.api.settings.CMAccountSettings

case class CMAccountState(
    id: String,
    `type`: CMAccountType,
    label: Option[String],
    displayName: Option[String],
    statements: List[CMStatement]
) {

  def forPeriod(period: Period): CMAccountState = {
    val statementsAfterStartPeriod = statements
      .sorted(CMStatement.ORDER_ASC)
      .dropWhile(_.date.isBefore(period.startDate))

    val statementsForPeriod = period.endDate match {
      case Some(endDate) =>
        statementsAfterStartPeriod.takeWhile(
          _.date.isBefore(endDate.plusDays(1))
        )
      case None =>
        statementsAfterStartPeriod
    }

    this.copy(statements = statementsForPeriod)
  }

  def toOverview: CMAccountOverview = {
    val result = statements.lastOption.map(_.balance).getOrElse {
      sys.error(s"Unable to get balance for account ${id}")
    }

    CMAccountOverview(
      id,
      `type`,
      label,
      displayName,
      result
    )
  }
}

object CMAccountState {

  def apply(
      accountSettings: CMAccountSettings,
      statements: List[CMStatement]
  ): CMAccountState = {
    CMAccountState(
      id = accountSettings.id,
      `type` = accountSettings.`type`,
      label = None,
      displayName = Some(accountSettings.label),
      statements = statements
    )
  }

  def apply(
      id: String,
      `type`: String,
      label: Option[String],
      displayName: Option[String],
      statements: List[CMStatement]
  ): CMAccountState = {
    CMAccountState(
      id = id,
      `type` = CMAccountType(`type`),
      label = label,
      displayName = displayName,
      statements = statements
    )
  }

  def unknown(
      id: String,
      label: Option[String],
      statements: List[CMStatement]
  ): CMAccountState = {
    CMAccountState(
      id,
      CMAccountType.Unknown,
      label,
      displayName = None,
      statements
    )
  }

  implicit val encoder: Encoder[CMAccountState] =
    deriveEncoder[CMAccountState]

  implicit def entityEncoder[F[_]: Sync]: EntityEncoder[F, CMAccountState] =
    jsonEncoderOf[CMAccountState]

  implicit def entitiesEncoder[F[_]: Sync]
      : EntityEncoder[F, List[CMAccountState]] =
    jsonEncoderOf[List[CMAccountState]]
}
