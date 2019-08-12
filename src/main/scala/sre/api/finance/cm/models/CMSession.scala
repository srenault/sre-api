package sre.api.finance.cm

import org.http4s.{ RequestCookie, ResponseCookie }
import org.http4s.headers.Cookie

case class CMSession(cookie: ResponseCookie) {
  def toRequestCookie: Cookie = {
    Cookie(RequestCookie(cookie.name, cookie.content))
  }
}
