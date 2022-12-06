package sre.api.domoticz

import org.http4s.Uri

case class DomoticzSettings(
  baseUri: Uri,
  wsUri: Uri,
  username: String,
  password: String
)
