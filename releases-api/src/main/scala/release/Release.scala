package sre.api.releases

import java.time.LocalDateTime
import cats.effect._
import org.http4s.EntityEncoder
import org.http4s.circe._
import io.circe.Encoder
import io.circe.generic.semiauto._

case class Release(name: String, versions: List[Version])

object Release {
  implicit val encoder: Encoder[Release] = deriveEncoder[Release]
  implicit def entitiesEncoder[F[_]: Concurrent]: EntityEncoder[F, List[Release]] = jsonEncoderOf[F, List[Release]]
}
