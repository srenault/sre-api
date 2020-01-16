import org.scalatest._
import cats.effect._
import cats.implicits._
import cats.effect.testing.scalatest.AsyncIOSpec
import sre.api._
import sre.api.finance._
import sre.api.finance.analytics._

class AnalyticsClientSpec extends AsyncIOSpec with Matchers {

  lazy val settings: Settings = Settings.load() match {
    case Right(settings) => settings
    case Left(error) => sys.error(s"Unable to load settings: $error")
  }

  lazy val streamAnalyticsClient = for {
    icomptaClient <- icompta.IComptaClient.stream[IO](settings)
    indexClient = AnalyticsIndexClient[IO](icomptaClient, settings)
    dbClient <- DBClient.stream[IO](settings)
  } yield {
    AnalyticsClient[IO](indexClient, icomptaClient, dbClient, settings)
  }

  "AnalyticsClient" - {
    "should reindex all ofx files" in {
      (for {
        analyticsClient <- streamAnalyticsClient.compile.lastOrError

        periodIndexes <- analyticsClient.reindex(fromScratch = true)

        statementsByPeriod <- periodIndexes.collect {
          case periodIndex: CompletePeriodIndex =>
            analyticsClient.getStatementsForPeriod(periodIndex.yearMonth).value.map(periodIndex -> _)
        }.sequence
      } yield {
        statementsByPeriod.foreach {
          case (period, Some((_, statements))) =>
            period.balance shouldBe statements.foldLeft(0D)(_ + _.amount)

          case (period, _) =>
            sys.error(s"Missing period ${period.yearMonth} in database")
        }
      })
    }
  }
}
