package sre.api.heaters

import java.util.Base64
import cats._
import org.http4s._
import org.http4s.client._
import org.http4s.dsl.io._
import org.http4s.client.dsl.Http4sClientDsl
import sre.api.HeatersSettings
import org.typelevel.ci._

trait HeatersClientDsl[F[_]] extends Http4sClientDsl[F] {

  def httpClient: Client[F]

  def settings: HeatersSettings

  def AuthenticatedGET(uri: Uri)(implicit F: Monad[F]): F[Request[F]] = {
    val authHeader = {
      val value = s"${settings.username}:${settings.password}"
      val encodedValue = Base64.getEncoder().encodeToString(value.getBytes("UTF-8"))
      Header.Raw(ci"Authorization", s"Basic $encodedValue")
    }
    GET(uri, authHeader)
  }
}
