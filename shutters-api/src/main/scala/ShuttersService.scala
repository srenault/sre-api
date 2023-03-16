package sre.api.shutters

import scala.collection.SortedSet
import cats.effect._
import cats.implicits._
import io.circe._
import io.circe.literal._
import sre.api.domoticz.{DomoticzClient, SwitchCmd}
import sre.api.shutters.Action
import sre.api.settings.ShuttersSettings

class ShuttersService[F[_]: Async](
    domoticzClient: DomoticzClient[F],
    settings: ShuttersSettings
) {

  def update(id: Int, action: Action): F[Boolean] = {
    action match {
      case Action.Open =>
        domoticzClient.switchLightCmd(id, SwitchCmd.Off)

      case Action.Close =>
        domoticzClient.switchLightCmd(id, SwitchCmd.On)

      case Action.Stop =>
        domoticzClient.switchLightCmd(id, SwitchCmd.Stop)
    }
  }
}
