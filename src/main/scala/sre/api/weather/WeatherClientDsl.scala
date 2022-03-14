package sre.api.weather

import cats._
import org.http4s._
import org.http4s.client._
import org.http4s.dsl.io._
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.dsl.impl.Path
import sre.api.WeatherSettings
import sre.api.utils.Security

trait WeatherClientDsl[F[_]] extends Http4sClientDsl[F] {

  def httpClient: Client[F]

  def settings: WeatherSettings

  val DEVICE_ID = "bd57f205de58de94"

  val CONTENT_TYPE_HEADER = Header("Content-Type", "application/json")

  def computeApiKey(path: Path): String = {
    val timestamp = System.currentTimeMillis / 1000
    val p = path.toList.mkString("/")
    val str = s"${DEVICE_ID}s2Ubrepu@-Aw${timestamp}$p"
    val paramString = Security.md5(str).replace("+", "-").replace("/", "_").replace("=", "")
    s"${paramString}|${timestamp}"
  }

  def AuthenticatedGET(endpoint: Uri, path: Path)(implicit F: Monad[F]): F[Request[F]] = {
    val apiKey = computeApiKey(path)
    val baseUri = endpoint / apiKey / DEVICE_ID / "get"
    val uri = path.toList.foldLeft(baseUri)(_ / _)
    GET(uri, CONTENT_TYPE_HEADER)
  }
}
