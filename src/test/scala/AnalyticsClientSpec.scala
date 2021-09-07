import org.scalatest.matchers.should.Matchers
import cats.effect._
import cats.implicits._
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.freespec.AsyncFreeSpec
import sre.api._
import sre.api.finance._
import sre.api.finance.analytics._

class AnalyticsClientSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {

  private def round(n: Double): Float = BigDecimal(2).setScale(0, BigDecimal.RoundingMode.HALF_UP).toFloat

  lazy val settings: Settings = Settings.load() match {
    case Right(settings) => settings
    case Left(error) => sys.error(s"Unable to load settings: $error")
  }

  lazy val streamAnalyticsClient = for {
    icomptaClient <- icompta.IComptaClient.stream[IO](settings)
    dbClient <- DBClient.stream[IO](settings)
    indexClient = AnalyticsIndexClient[IO](icomptaClient, dbClient, settings)
  } yield {
    AnalyticsClient[IO](indexClient, icomptaClient, dbClient, settings)
  }

  "AnalyticsClient" - {
    "should reindex all ofx files" in {
      for {
        analyticsClient <- streamAnalyticsClient.compile.lastOrError

        periodIndexes <- analyticsClient.reindex(fromScratch = true)

        completePeriodIndexes = periodIndexes.collect {
          case periodIndex: CompletePeriodIndex => periodIndex
        }.sorted(PeriodIndex.ORDER_DESC)

        statementsByPeriod <- completePeriodIndexes.map { periodIndex =>
          analyticsClient.getStatementsForPeriod(periodIndex.yearMonth).value.map(periodIndex -> _)
        }.sequence
      } yield {
        // Check results
        statementsByPeriod.foreach {
          case (period, Some((_, statements))) =>
            val result = statements.foldLeft(0D)(_ + _.amount)
            period.result shouldBe result

          case (period, _) =>
            sys.error(s"Missing period ${period.yearMonth} in database")
        }

        // Check balances
        val balancesFromResults = completePeriodIndexes.map(periodIndex => periodIndex.totalBalance - periodIndex.result)
        val balancesFromStatements = completePeriodIndexes.tail.map(_.totalBalance)
        balancesFromResults.zip(balancesFromStatements).foreach {
          case (balanceFromResult, balanceFromStatement) =>
            round(balanceFromResult) shouldBe round(balanceFromStatement)
        }
      }
    }
  }
}
