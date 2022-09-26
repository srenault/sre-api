package sre.api.heaters

import scala.collection.SortedSet
import scala.xml.Elem
import cats.effect._
import cats.implicits._
import org.http4s.client._
import org.http4s.scalaxml._
import sre.api.settings.HeatersSettings

case class HeatersClient[F[_]: Async](httpClient: Client[F], settings: HeatersSettings) extends HeatersClientDsl[F] {

  def getStatus(): F[SortedSet[ChannelStatus]] = {
    val uri = (settings.baseUri / "Q")
    val request = AuthenticatedGET(uri)
    httpClient.expect[Elem](request).map(ChannelStatus.readsSet)
  }

  def update(channel: Int, mode: Mode): F[SortedSet[ChannelStatus]] = {
    val uri = (settings.baseUri / s"F${channel}${mode.id}")
    val request = AuthenticatedGET(uri)
    httpClient.expect[Elem](request).map(ChannelStatus.readsSet)
  }
}
