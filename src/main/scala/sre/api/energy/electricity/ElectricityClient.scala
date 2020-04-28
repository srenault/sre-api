package sre.api
package energy
package electricity

import java.time.temporal.ChronoUnit
import java.time.LocalDate
import cats.effect._
import cats.Parallel
import cats.implicits._
import fs2.Stream
import domoticz._

case class ElectricityClient[F[_]: Effect : Parallel](domoticzClient: DomoticzClient[F], implicit val settings: Settings) {

  def getDailyConsumption(dateFrom: LocalDate, dateTo: LocalDate): F[List[PowerConsumption]] = {
    val range = Range.Period(dateFrom, dateTo)

    val eventuallyHcValues = domoticzClient.graph[PowerMeter1](
      sensor = domoticz.Sensor.Counter,
      idx = 1,
      range = range
    )

    val eventuallyHpValues = domoticzClient.graph[PowerMeter1](
      sensor = domoticz.Sensor.Counter,
      idx = 2,
      range = range
    )

    (eventuallyHcValues, eventuallyHpValues).parMapN { (hcValues, hpValues) =>
      val hcConsumption = hcValues.map {
        case PowerMeter1(date, hc) =>
          val hcCost = Electricity.computeHcCost(settings.energy.electricity, hc)
          PowerConsumption(date, hp = 0, hc, hpCost = 0, hcCost)
      }

      val hpConsumption = hpValues.map {
        case PowerMeter1(date, hp) =>
          val hpCost = Electricity.computeHpCost(settings.energy.electricity, hp)
          PowerConsumption(date, hp, hc = 0, hpCost, hcCost = 0)
      }

      (hcConsumption ++ hpConsumption).groupBy(_.date).flatMap {
        case (_, consumptions) =>
          consumptions.foldLeft[Option[PowerConsumption]](None) {
            case (None, consumption) => Some(consumption)
            case (Some(consumptionA), consumptionB) =>
              Some(
                consumptionA.copy(
                  hc = consumptionA.hc + consumptionB.hc,
                  hp = consumptionA.hp + consumptionB.hp,
                  hpCost = consumptionA.hpCost + consumptionB.hpCost,
                  hcCost = consumptionA.hcCost + consumptionB.hcCost
                )
              )
          }
      }.toList
    }
  }

  def getMontlyConsumption(): F[List[PowerConsumption]] = {
    domoticzClient.graph[PowerMeter2](
      sensor = domoticz.Sensor.Counter,
      idx = 3,
      range = Range.Month
    ).map { values =>
      values.map {
        case PowerMeter2(date, hp, hc) =>
          val hpCost = Electricity.computeHpCost(settings.energy.electricity, hp)
          val hcCost = Electricity.computeHcCost(settings.energy.electricity, hc)
          PowerConsumption(date, hp, hc, hpCost, hcCost)
      }
    }
  }

  def getLatestLoad(): F[List[Load]] = {
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
          case event: websocket.events.hardware.Teleinfo if !event.isUnknown => event
        })

      case _ => Stream.empty
    }
  }
}

object Electricity {

  private def round(n: Float): Float = scala.math.round(n * 100F) / 100F

  def computeCost(settings: ElectricitySettings, consumption: List[PowerConsumption]): Float = {
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

  def computeHcCost(settings: ElectricitySettings, hcTotal: Float): Float = {
    val hcCost = round(hcTotal * settings.ratio.hc)
    round(hcCost)
  }

  def computeHpCost(settings: ElectricitySettings, hpTotal: Float): Float = {
    val hpCost = round(hpTotal * settings.ratio.hp)
    round(hpCost)
  }

  def computeConsumptionCost(settings: ElectricitySettings, hcTotal: Float, hpTotal: Float): Float = {
    val hcCost = computeHcCost(settings, hcTotal)

    val hpCost = computeHpCost(settings, hpTotal)

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
