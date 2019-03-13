package sre.dashboard

import java.time.LocalDate
import io.circe.literal._
import cats.Apply
import cats.effect._
import cats.implicits._
import cats.data._
import cats.data.Validated._
import org.http4s._
import org.http4s.circe._
import domoticz.DomoticzClient
import energy._

class EnergyService[F[_]: Effect](domoticzClient: DomoticzClient[F], settings: Settings) extends EnergyServiceDsl[F] {

  private def round(n: Float): Float =
    scala.math.round(n * 100F) / 100F

  val service: HttpService[F] = {
    HttpService[F] {
      case GET -> Root / "electricity" :? DateFromQueryParamMatcher(maybeDateFrom) +& DateToQueryParamMatcher(maybeDateTo) =>

        val defaultDateFrom = LocalDate.now().withDayOfMonth(1)
        val defaultDateTo = LocalDate.now()

        val maybePeriod = Apply[ValidatedNel[ParseFailure, ?]].map2(
          maybeDateFrom.getOrElse(defaultDateFrom.validNel),
          maybeDateTo.getOrElse(defaultDateTo.validNel)) {
          case (dateFrom, dateTo) => domoticz.Range.Period(dateFrom, dateTo)
        }

        maybePeriod match {

          case Valid(period) =>
            domoticzClient.graph[ElectrictyConsumption](
              sensor = domoticz.Sensor.Counter,
              idx = settings.domoticz.teleinfo.idx,
              range = period
            ).flatMap { consumptionByDay =>

              val (hcTotal, hpTotal) = consumptionByDay.foldLeft(0F -> 0F) {
                case ((hcAcc, hpAcc), consumption) =>
                  (hcAcc + consumption.hc) -> (hpAcc + consumption.hp)
              }

              val electricitySettings = settings.energy.electricity

              val hcCost = round(hcTotal * electricitySettings.ratio.hc)
              val hpCost = round(hpTotal * electricitySettings.ratio.hp)

              val totalKwh = hcTotal + hpTotal
              val taxeCommunaleCost = round(totalKwh * electricitySettings.ratio.taxeCommunale)
              val taxeDepartementaleCost = round(totalKwh * electricitySettings.ratio.taxeDepartementale)
              val cspeCost = round(totalKwh * electricitySettings.ratio.cspe)

              val ctaCost = electricitySettings.monthlyCta * electricitySettings.ratio.cta
              val tvaReduiteCost = round((electricitySettings.monthlySubscription + ctaCost) * electricitySettings.ratio.tvaReduite)
              val tvaCost = round((hpCost + hcCost + taxeCommunaleCost + taxeDepartementaleCost + cspeCost) * electricitySettings.ratio.tva)
              val cost = round(electricitySettings.monthlySubscription + hcCost + hpCost + taxeCommunaleCost + taxeDepartementaleCost + cspeCost + ctaCost + tvaReduiteCost + tvaCost)

              Ok(json"""{ "cost": $cost }""")
            }

          case Invalid(errors) =>
            val json = errors.map(_.message)
            BadRequest(json""" { "errors" : $json }""")
        }
    }
  }
}
