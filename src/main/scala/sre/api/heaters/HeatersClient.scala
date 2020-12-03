package sre.api
package heaters

import scala.xml.Elem
import cats.effect._
import cats.implicits._
import org.http4s.client._
import org.http4s.scalaxml._

case class HeatersClient[F[_]: ConcurrentEffect](httpClient: Client[F], settings: HeatersSettings) extends HeatersClientDsl[F] {

  def getStatus(): F[ChannelsStatus] = {
    val uri = (settings.baseUri / "Q")
    val request = AuthenticatedGET(uri)
    httpClient.expect[Elem](request).map(ChannelsStatus.reads)
  }

  def update(channel: Int, mode: Mode): F[ChannelsStatus] = {
    val uri = (settings.baseUri / s"F${channel}${mode.id}")
    val request = AuthenticatedGET(uri)
    httpClient.expect[Elem](request).map(ChannelsStatus.reads)
  }
}
