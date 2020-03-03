package sre.api
package energy
package electricity

import java.time.temporal.ChronoUnit
import java.time.LocalDate
import cats.effect._
import fs2.Stream
import domoticz._

case class ElectricityClient[F[_]: Effect](domoticzClient: DomoticzClient[F], implicit val settings: Settings) {

  def getDailyConsumption(dateFrom: LocalDate, dateTo: LocalDate): F[List[Consumption]] = {
    domoticzClient.graph[Consumption](
      sensor = domoticz.Sensor.Counter,
      idx = 3,
      range = Range.Period(dateFrom, dateTo)
    )
  }

  def getMontlyConsumption(): F[List[Consumption]] = {
    domoticzClient.graph[Consumption](
      sensor = domoticz.Sensor.Counter,
      idx = 3,
      range = Range.Month
    )
  }

  def getCurrentLoad(): F[List[Load]] = {
    domoticzClient.graph[Load](
      sensor = domoticz.Sensor.Percentage,
      idx = 5,
      range = Range.Day
    )
  }

  def streamTeleinfo(): Stream[F, websocket.events.hardware.Teleinfo] = {
    domoticzClient.wsTopic.subscribe(10).flatMap {
      case message: websocket.messages.Response.Device =>
        Stream.emits(message.events.collect {
          case t: websocket.events.hardware.Teleinfo => t
        })

      case _ => Stream.empty
    }
  }
}

object Electricity {

  private def round(n: Float): Float = scala.math.round(n * 100F) / 100F

  def computeCost(settings: ElectricitySettings, consumption: List[Consumption]): Float = {
    (for {
      dateFrom <- consumption.headOption.map(_.date)
      dateTo <- consumption.lastOption.map(_.date)
    } yield {
      val (hcTotal, hpTotal) = consumption.foldLeft(0F -> 0F) {
        case ((hcAcc, hpAcc), consumption) =>
          (hcAcc + consumption.hc) -> (hpAcc + consumption.hp)
      }

      computeCost(settings, dateFrom, dateTo, hcTotal, hpTotal)
    }) getOrElse 0F
  }

  def computeConsumptionCost(settings: ElectricitySettings, hcTotal: Float, hpTotal: Float): Float = {
    val hcCost = round(hcTotal * settings.ratio.hc)

    val hpCost = round(hpTotal * settings.ratio.hp)

    round(hcCost + hpCost)
  }

  def computeCost(settings: ElectricitySettings, dateFrom: LocalDate, dateTo: LocalDate, hcTotal: Float, hpTotal: Float): Float = {
    val consumptionCost = computeConsumptionCost(settings, hcTotal, hpTotal)

    val nbMonths = scala.math.round(ChronoUnit.DAYS.between(dateFrom, dateTo).toFloat / 30F).toInt

    val subscriptionCost = (settings.yearlySubscription / 12F) * nbMonths

    val totalKwh = hcTotal + hpTotal

    val taxeCommunaleCost = round(totalKwh * settings.ratio.taxeCommunale)

    val taxeDepartementaleCost = round(totalKwh * settings.ratio.taxeDepartementale)

    val ctaCost = settings.monthlyCta * nbMonths * settings.ratio.cta

    val cspeCost = round(totalKwh * settings.ratio.cspe)

    val tvaReduiteCost = round((subscriptionCost + ctaCost) * settings.ratio.tvaReduite)

    val tvaCost = round((consumptionCost + taxeCommunaleCost + taxeDepartementaleCost + cspeCost) * settings.ratio.tva)

    round(subscriptionCost + consumptionCost + taxeCommunaleCost + taxeDepartementaleCost + cspeCost + ctaCost + tvaReduiteCost + tvaCost)
  }
}
