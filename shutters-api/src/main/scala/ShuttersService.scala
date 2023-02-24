package sre.api.shutters

import scala.collection.SortedSet
import cats.effect._
import cats.implicits._
import io.circe._
import io.circe.literal._
import sre.api.domoticz.{DomoticzClient, SwitchCmd}
import sre.api.shutters.State
import sre.api.settings.ShuttersSettings

class ShuttersService[F[_]: Async](
    domoticzClient: DomoticzClient[F],
    settings: ShuttersSettings
) {

  def update(id: Int, state: State): F[Boolean] = {
    state match {
      case State.Opened =>
        domoticzClient.switchLightCmd(id, SwitchCmd.Off)

      case State.Closed =>
        domoticzClient.switchLightCmd(id, SwitchCmd.On)

      case State.Stopped =>
        domoticzClient.switchLightCmd(id, SwitchCmd.Stop)
    }
  }
}
