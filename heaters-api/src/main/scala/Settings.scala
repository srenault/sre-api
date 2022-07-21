package sre.api.heaters

import org.http4s.Uri

case class Settings(
  httpClient: HttpClientSettings,
  heaters: HeatersSettings
)

object Settings {

  lazy val env = System.getenv

  private def getString(key: String): Option[String] =
    Option(env.get(key))

  private def getStringOrFail(key: String): String =
    getString(key) getOrElse sys.error(s"Configuration error: Unable to get $key")

  private def getUri(key: String): Option[Uri] =
    getString(key).flatMap(Uri.fromString(_).toOption)

  private def getUriOrFail(key: String): Uri =
    getUri(key) getOrElse sys.error(s"Configuration error: Unable to get $key")

  private def getBoolean(key: String): Option[Boolean] =
    scala.util.Try(getStringOrFail(key).toBoolean).toOption

  private def getBooleanOrFail(key: String): Boolean =
    getBoolean(key) getOrElse {
      sys.error(s"Configuration error: Unable to get $key as Boolean")
    }


  def build(): Settings = {
    Settings(
      httpClient = HttpClientSettings(
        logRequest = getBooleanOrFail("HTTPCLIENT_LOGREQUEST"),
        logResponse = getBooleanOrFail("HTTPCLIENT_LOGRESPONSE")
      ),
      heaters = HeatersSettings(
        baseUri = getUriOrFail("HEATERS_BASEURI"),
        username = getStringOrFail("HEATERS_USERNAME"),
        password = getStringOrFail("HEATERS_PASSWORD")
      )
    )
  }
}

case class HeatersSettings(
  baseUri: Uri,
  username: String,
  password: String
)

case class HttpClientSettings(
  logRequest: Boolean,
  logResponse: Boolean
)
