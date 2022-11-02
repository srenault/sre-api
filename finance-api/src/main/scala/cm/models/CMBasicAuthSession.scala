package sre.api.finance.cm

import cats.data.NonEmptyList
import org.http4s.{RequestCookie, ResponseCookie}

case class CMBasicAuthSession(
    idSesCookie: RequestCookie,
    otherCookies: List[RequestCookie]
) {
  def cookies: NonEmptyList[RequestCookie] =
    NonEmptyList(idSesCookie, otherCookies)
}

object CMBasicAuthSession {
  val IDSES_COOKIE = "IdSes"

  def create(
      idSesResponseCookie: ResponseCookie,
      otherResponseCookies: List[ResponseCookie]
  ): CMBasicAuthSession = {
    val idSesCookie =
      RequestCookie(idSesResponseCookie.name, idSesResponseCookie.content)
    val otherCookies = otherResponseCookies.map(cookie =>
      RequestCookie(cookie.name, cookie.content)
    )
    CMBasicAuthSession(idSesCookie, otherCookies)
  }
}
