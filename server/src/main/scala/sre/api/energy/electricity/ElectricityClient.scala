package sre.api
package energy
package electricity

import java.time.temporal.ChronoUnit
import java.time.{LocalDate, Year, Month}
import cats.effect._
import cats.Parallel
import cats.implicits._
import fs2.Stream
import domoticz._

case class ElectricityClient[F[_]: Async: Parallel](
    domoticzClient: DomoticzClient[F],
    implicit val settings: Settings
) {

  def getDailyConsumption(
      dateFrom: LocalDate,
      dateTo: LocalDate
  ): F[PowerConsumption] = {
    val eventuallyCountersByDate = if (dateFrom.getYear == dateTo.getYear) {
      val year = Year.from(dateFrom)
      getYearlyConsumption(maybeYear = Some(year)).map { yearConsumption =>
        yearConsumption
          .map(consumption => consumption.date -> consumption)
          .toMap
      }
    } else {
      val startYear = Year.from(dateFrom)
      val endYear = Year.from(dateTo)

      val eventuallyStartYear =
        getYearlyConsumption(maybeYear = Some(startYear))
      val eventuallyEndYear = getYearlyConsumption(maybeYear = Some(endYear))

      (eventuallyStartYear, eventuallyEndYear).parMapN {
        case (startYearConsumption, endYearConsumption) =>
          val startYearConsumptionByDate = startYearConsumption.map {
            consumption =>
              consumption.date -> consumption
          }.toMap

          val endYearConsumptionByDate = endYearConsumption.map { consumption =>
            consumption.date -> consumption
          }.toMap

          startYearConsumptionByDate ++ endYearConsumptionByDate
      }
    }

    val range = Range.Period(dateFrom, dateTo)

    val eventuallyHcValues = domoticzClient
      .graph[PowerMeter1](
        sensor = domoticz.Sensor.Counter,
        idx = 1,
        range = range
      )
      .map(_.dropRight(1))

    val eventuallyHpValues = domoticzClient
      .graph[PowerMeter1](
        sensor = domoticz.Sensor.Counter,
        idx = 2,
        range = range
      )
      .map(_.dropRight(1))

    (eventuallyCountersByDate, eventuallyHcValues, eventuallyHpValues).parMapN {
      case (countersByDate, hcValues, hpValues) =>
        val dailyUsage: List[PowerUsage] =
          hcValues.zip(hpValues).map { case (hcUsage, hpUsage) =>
            PowerUsage(hpUsage.date, hp = hpUsage.value, hc = hcUsage.value)
          }

        (for {
          firstUsage <- dailyUsage.headOption

          (startHcCounter, startHpCounter) <- countersByDate
            .get(firstUsage.date)
            .map { usage =>
              usage.hcCounter -> usage.hpCounter
            }

          lastUsage <- dailyUsage.lastOption

          (endHcCounter, endHpCounter) <- countersByDate
            .get(lastUsage.date)
            .map { usage =>
              usage.hcCounter -> usage.hpCounter
            }
        } yield {
          PowerConsumption(
            startHcCounter = startHcCounter,
            endHcCounter = endHcCounter + lastUsage.hc,
            startHpCounter = startHpCounter,
            endHpCounter = endHpCounter + lastUsage.hp,
            dailyUsage
          )
        }) getOrElse PowerConsumption.empty
    }
  }

  private def getYearlyConsumption(
      maybeYear: Option[Year]
  ): F[List[PowerMeter3]] = {
    domoticzClient.graph[PowerMeter3](
      sensor = domoticz.Sensor.Counter,
      idx = 3,
      range = Range.Year,
      maybeActYear = maybeYear,
      maybeActMonth = None
    )
  }

  def getLastMonthConsumption(): F[PowerConsumption] = {
    val now = LocalDate.now();
    val month = Month.from(now)

    domoticzClient
      .graph[PowerMeter3](
        sensor = domoticz.Sensor.Counter,
        idx = 3,
        range = Range.Month,
        maybeActYear = None,
        maybeActMonth = Some(month)
      )
      .map { consumption =>
        val dailyUsage = consumption.map {
          case PowerMeter3(date, _, hpUsage, _, hcUsage) =>
            PowerUsage(date, hpUsage, hcUsage)
        }

        val (startHcCounter, endHcCounter, startHpCounter, endHpCounter) =
          (for {
            first <- consumption.headOption
            last <- consumption.lastOption
          } yield {
            val startHc = first.hcCounter
            val endHc = last.hcCounter + last.hcUsage

            val startHp = first.hpCounter
            val endHp = last.hpCounter + last.hpUsage

            (startHc, endHc, startHp, endHp)

          }).getOrElse((0f, 0f, 0f, 0f))

        PowerConsumption(
          startHcCounter = startHcCounter,
          endHcCounter = endHpCounter,
          startHpCounter = startHpCounter,
          endHpCounter = endHpCounter,
          dailyUsage
        )
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
          case event: websocket.events.hardware.Teleinfo if !event.isUnknown =>
            event
        })

      case _ => Stream.empty
    }
  }
}

object Electricity {

  private def round(n: Float): Float = scala.math.round(n * 100f) / 100f

  def computeHcCost(settings: ElectricitySettings, hcTotal: Float): Float = {
    val hcCost = round(hcTotal * settings.ratio.hc)
    round(hcCost)
  }

  def computeHpCost(settings: ElectricitySettings, hpTotal: Float): Float = {
    val hpCost = round(hpTotal * settings.ratio.hp)
    round(hpCost)
  }

  def computeCost(
      settings: ElectricitySettings,
      consumption: PowerConsumption
  ): Float = {
    computeCost(settings, consumption.hcTotalUsage, consumption.hpTotalUsage)
  }

  def computeCost(
      settings: ElectricitySettings,
      hcTotal: Float,
      hpTotal: Float
  ): Float = {
    val hcCost = computeHcCost(settings, hcTotal)

    val hpCost = computeHpCost(settings, hpTotal)

    round(hcCost + hpCost)
  }

  def computeCostWithTaxes(
      settings: ElectricitySettings,
      consumption: PowerConsumption
  ): Float = {
    (for {
      first <- consumption.dailyUsage.headOption
      last <- consumption.dailyUsage.lastOption
    } yield {
      val dateFrom = first.date
      val dateTo = last.date
      computeCostWithTaxes(
        settings,
        dateFrom,
        dateTo,
        consumption.hcTotalUsage,
        consumption.hpTotalUsage
      )
    }) getOrElse 0f
  }

  def computeCostWithTaxes(
      settings: ElectricitySettings,
      dateFrom: LocalDate,
      dateTo: LocalDate,
      hcTotal: Float,
      hpTotal: Float
  ): Float = {
    val consumptionCost = computeCost(settings, hcTotal, hpTotal)

    val nbMonths = scala.math
      .round(ChronoUnit.DAYS.between(dateFrom, dateTo).toFloat / 30f)
      .toInt

    val subscriptionCost = (settings.yearlySubscription / 12f) * nbMonths

    val totalKwh = hcTotal + hpTotal

    val taxeCommunaleCost = round(totalKwh * settings.ratio.taxeCommunale)

    val taxeDepartementaleCost = round(
      totalKwh * settings.ratio.taxeDepartementale
    )

    val ctaCost = settings.monthlyCta * nbMonths * settings.ratio.cta

    val cspeCost = round(totalKwh * settings.ratio.cspe)

    val tvaReduiteCost = round(
      (subscriptionCost + ctaCost) * settings.ratio.tvaReduite
    )

    val tvaCost = round(
      (consumptionCost + taxeCommunaleCost + taxeDepartementaleCost + cspeCost) * settings.ratio.tva
    )

    round(
      subscriptionCost + consumptionCost + taxeCommunaleCost + taxeDepartementaleCost + cspeCost + ctaCost + tvaReduiteCost + tvaCost
    )
  }
}
