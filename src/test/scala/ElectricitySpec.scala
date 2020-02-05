import java.time.LocalDate
import org.scalatest._
import sre.api._
import sre.api.energy.Electricity

class ElectricitySpec extends FlatSpec with Matchers {

  case class Input(dateFrom: LocalDate, dateTo: LocalDate, hcTotal: Float, hpTotal: Float, expectedCost: Float)

  lazy val inputs = List(
    Input(
      dateFrom = LocalDate.parse("2018-10-01"),
      dateTo = LocalDate.parse("2019-06-30"),
      hcTotal = 4048,
      hpTotal = 8049,
      expectedCost = 1897F
    ),
    Input(
      dateFrom = LocalDate.parse("2018-04-13"),
      dateTo = LocalDate.parse("2018-07-16"),
      hcTotal = 68,
      hpTotal = 59,
      expectedCost = 60F
    ),
    Input(
      dateFrom = LocalDate.parse("2018-01-25"),
      dateTo = LocalDate.parse("2018-04-13"),
      hcTotal = 53,
      hpTotal = 294,
      expectedCost = 97F
    )
  )

  lazy val settings: Settings = Settings.load() match {
    case Right(settings) => settings
    case Left(error) => sys.error(s"Unable to load settings: $error")
  }

  "Electricity" should "compute cost following consumption" in {
    for(input <- inputs) yield {
      val cost = Electricity.computeCost(
        settings.energy.electricity,
        input.dateFrom,
        input.dateTo,
        input.hcTotal,
        input.hpTotal
      )
      println(input)
      cost should be (input.expectedCost.toFloat +- 10F)
    }
  }
}
