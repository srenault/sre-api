package sre.api

import java.time.LocalDate
import io.circe.syntax._
import io.circe.literal._
import cats.Apply
import cats.effect._
import cats.implicits._
import cats.data._
import cats.data.Validated._
import org.http4s._
import org.http4s.circe._
import energy._
import energy.electricity.Electricity
import fs2.Stream

class EnergyService[F[_]](energyClient: EnergyClient[F], settings: Settings)(implicit F: Effect[F]) extends EnergyServiceDsl[F] {

  val service: HttpService[F] = CorsMiddleware(settings) {
    HttpService[F] {

      case GET -> Root / "electricity" / "consumption" :? DateFromQueryParamMatcher(maybeValidDateFrom) +& DateToQueryParamMatcher(maybeValidDateTo) =>
        val maybeValidPeriod = Apply[ValidatedNel[ParseFailure, ?]].map2(maybeValidDateFrom.sequence, maybeValidDateTo.sequence)(_ -> _)

        maybeValidPeriod match {
          case Invalid(errors) =>
            val json = errors.map(_.message)

            BadRequest(json""" { "errors" : $json }""")

          case Valid((None, None)) =>
            energyClient.electricity.getMontlyConsumption().flatMap { consumption =>
              val totalCost = Electricity.computeCost(settings.energy.electricity, consumption)
              Ok(json"""{ "consumption": $consumption, "totalCost": $totalCost }""")
            }

          case Valid((maybeDateFrom, maybeDateTo)) =>
            val dateTo = maybeDateTo getOrElse LocalDate.now
            val dateFrom = maybeDateFrom getOrElse dateTo.minusMonths(6)

            energyClient.electricity.getDailyConsumption(dateFrom, dateTo).flatMap { consumption =>
              val totalCost = Electricity.computeCost(settings.energy.electricity, consumption)
              Ok(json"""{ "consumption": $consumption, "totalCost": $totalCost }""")
            }
        }

      case GET -> Root / "electricity" / "latest" / "load" =>
        energyClient.electricity.getLatestLoad().flatMap { result =>
          Ok(json"""{ "result": $result }""")
        }

      case GET -> Root / "electricity" / "stream" =>
        val stream: Stream[F, ServerSentEvent]  = energyClient.electricity.streamTeleinfo().map { event =>
          ServerSentEvent(event.asJson.noSpaces)
        }
        Ok(stream)
    }
  }
}
