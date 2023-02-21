package sre.api.finance.cm

import org.http4s.{RequestCookie, ResponseCookie}
import org.http4s.headers.Cookie

case class CMAuthClientState(cookie: ResponseCookie) {
  def toRequestCookie: Cookie = {
    Cookie(RequestCookie(cookie.name, cookie.content))
  }
}

object CMAuthClientState {
  val id = "auth_client_state"
}
