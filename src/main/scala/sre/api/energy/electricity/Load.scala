package sre.api
package energy
package electricity

import java.time.LocalDateTime
import cats.effect._
import io.circe._
import org.http4s.EntityEncoder
import io.circe.Decoder
import io.circe.generic.semiauto._
import org.http4s.circe._

case class Load(date: LocalDateTime, value: Float)

object Load {

  implicit val localDateTimeDecoder = new Decoder[LocalDateTime] {
    final def apply(c: HCursor): Decoder.Result[LocalDateTime] = {
      val dateTimeFormatter = java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
      c.as[String].map(LocalDateTime.parse(_, dateTimeFormatter))
    }
  }

  implicit val decoder: Decoder[Load] = new Decoder[Load] {

    final def apply(c: HCursor): Decoder.Result[Load] =
      for {
        time <- c.downField("d").as[LocalDateTime](localDateTimeDecoder)
        value <- c.downField("v").as[String].map(_.toFloat)
      } yield Load(time, value)
  }

  implicit val encoder: Encoder[Load] = deriveEncoder[Load]
  implicit def entityEncoder[F[_]: Effect]: EntityEncoder[F, Load] = jsonEncoderOf[F, Load]
  implicit def entitiesEncoder[F[_]: Effect]: EntityEncoder[F, List[Load]] = jsonEncoderOf[F, List[Load]]
}
