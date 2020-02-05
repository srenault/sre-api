package sre.api

import io.circe.literal._
import cats.Apply
import cats.effect._
import cats.implicits._
import cats.data._
import cats.data.Validated._
import org.http4s._
import org.http4s.circe._
import energy._

class EnergyService[F[_]: Effect](energyClient: EnergyClient[F], settings: Settings) extends EnergyServiceDsl[F] {

  val service: HttpService[F] = CorsMiddleware(settings) {
    HttpService[F] {

      case GET -> Root / "electricity" / "consumption" :? DateFromQueryParamMatcher(dateFrom) +& DateToQueryParamMatcher(dateTo) =>

        val period = Apply[ValidatedNel[ParseFailure, ?]].map2(dateFrom, dateTo)(_ -> _)

        period match {
          case Valid((dateFrom, dateTo)) =>
            energyClient.getElectricityConsumption(dateFrom, dateTo).flatMap { consumption =>
              val cost = Electricity.computeCost(settings.energy.electricity, consumption)
              Ok(json"""{ "consumption": $consumption, "cost": $cost }""")
            }

          case Invalid(errors) =>
            val json = errors.map(_.message)
            BadRequest(json""" { "errors" : $json }""")
        }

      case GET -> Root / "electricity" / "current" / "load" =>
        energyClient.getCurrentElectricityLoad().flatMap { result =>
          Ok(json"""{ "result": $result }""")
        }
    }
  }
}
