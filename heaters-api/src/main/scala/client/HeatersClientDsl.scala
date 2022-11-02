package sre.api.heaters

import java.util.Base64
import cats._
import org.http4s._
import org.http4s.client._
import org.http4s.dsl.io._
import org.http4s.client.dsl.Http4sClientDsl
import sre.api.settings.HeatersSettings

trait HeatersClientDsl[F[_]] extends Http4sClientDsl[F] {

  def httpClient: Client[F]

  def settings: HeatersSettings

  def AuthenticatedGET(uri: Uri): Request[F] = {
    val authHeader = {
      val value = s"${settings.username}:${settings.password}"
      val encodedValue =
        Base64.getEncoder().encodeToString(value.getBytes("UTF-8"))
      Header("Authorization", s"Basic $encodedValue")
    }
    GET(uri, authHeader)
  }
}
