package sre.api
package energy

import java.time.temporal.ChronoUnit
import java.time.LocalDate
import cats.effect._
import cats.implicits._
import domoticz._

case class EnergyClient[F[_]: Effect](
  domoticzClient: DomoticzClient[F],
  settings: Settings
) {

  def getElectricityCostFor(dateFrom: LocalDate, dateTo: LocalDate): F[Float] = {
    domoticzClient.graph[ElectrictyConsumption](
      sensor = domoticz.Sensor.Counter,
      idx = settings.domoticz.teleinfo.idx,
      range = Range.Period(dateFrom, dateTo)
    ).map { consumptionByDay =>
      val (hcTotal, hpTotal) = consumptionByDay.foldLeft(0F -> 0F) {
        case ((hcAcc, hpAcc), consumption) =>
          (hcAcc + consumption.hc) -> (hpAcc + consumption.hp)
      }

      Electricity.computeCost(settings.energy.electricity)(dateFrom, dateTo, hcTotal, hpTotal)
    }
  }
}

object Electricity {

  def round(n: Float): Float = scala.math.round(n * 100F) / 100F

  def computeCost(settings: ElectricitySettings)(dateFrom: LocalDate, dateTo: LocalDate, hcTotal: Float, hpTotal: Float): Float = {
    val hcCost = round(hcTotal * settings.ratio.hc)

    val hpCost = round(hpTotal * settings.ratio.hp)

    val nbMonths = scala.math.round(ChronoUnit.DAYS.between(dateFrom, dateTo).toFloat / 30F).toInt

    val subscriptionCost = (settings.yearlySubscription / 12F) * nbMonths

    val totalKwh = hcTotal + hpTotal

    val taxeCommunaleCost = round(totalKwh * settings.ratio.taxeCommunale)

    val taxeDepartementaleCost = round(totalKwh * settings.ratio.taxeDepartementale)

    val ctaCost = settings.monthlyCta * nbMonths * settings.ratio.cta

    val cspeCost = round(totalKwh * settings.ratio.cspe)

    val tvaReduiteCost = round((subscriptionCost + ctaCost) * settings.ratio.tvaReduite)

    val tvaCost = round((hpCost + hcCost + taxeCommunaleCost + taxeDepartementaleCost + cspeCost) * settings.ratio.tva)

    round(subscriptionCost + hcCost + hpCost + taxeCommunaleCost + taxeDepartementaleCost + cspeCost + ctaCost + tvaReduiteCost + tvaCost)
  }
}
