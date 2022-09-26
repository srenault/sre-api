package sre.api.heaters

import scala.collection.SortedSet
import cats.effect._
import cats.implicits._
import io.circe._
import io.circe.literal._
import sre.api.settings.HeatersSettings

class HeatersService[F[_]: Async](heatersClient: HeatersClient[F], settings: HeatersSettings) {

  def getStatus(): F[SortedSet[ChannelStatus]] =
    heatersClient.getStatus()

  def update(channel: Int, mode: Mode): F[SortedSet[ChannelStatus]] =
    heatersClient.update(channel, mode)
}
