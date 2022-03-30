package sre.api.releases

import java.time.LocalDateTime
import cats.effect._
import org.http4s.EntityEncoder
import org.http4s.circe._
import io.circe.Encoder
import io.circe.generic.semiauto._

case class Version(sha1: String, date: LocalDateTime, url: String)

object Version {

  implicit val encoder: Encoder[Version] = deriveEncoder[Version]
}
