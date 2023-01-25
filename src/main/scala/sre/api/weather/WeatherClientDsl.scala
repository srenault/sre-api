package sre.api.weather

import org.typelevel.ci._
import org.http4s._
import org.http4s.client._
import org.http4s.dsl.io._
import org.http4s.client.dsl.Http4sClientDsl
import sre.api.WeatherSettings
import sre.api.utils.Security

trait WeatherClientDsl[F[_]] extends Http4sClientDsl[F] {

  def httpClient: Client[F]

  def settings: WeatherSettings

  val DEVICE_ID = "bd57f205de58de94"

  val CONTENT_TYPE_HEADER = Header.Raw(ci"Content-Type", "application/json")

  def computeApiKey(path: Uri.Path): String = {
    val timestamp = System.currentTimeMillis / 1000
    val p = path.segments.toList.mkString("/")
    val str = s"${DEVICE_ID}s2Ubrepu@-Aw${timestamp}$p"
    val paramString =
      Security.md5(str).replace("+", "-").replace("/", "_").replace("=", "")
    s"${paramString}|${timestamp}"
  }

  def AuthenticatedGET(endpoint: Uri, path: Uri.Path): Request[F] = {
    val apiKey = computeApiKey(path)
    val baseUri = endpoint / apiKey / DEVICE_ID / "get"
    val uri = path.segments.toList.foldLeft(baseUri)(_ / _)
    GET(uri, CONTENT_TYPE_HEADER)
  }
}
