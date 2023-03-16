import java.time.YearMonth
import org.scalatest.matchers.should.Matchers
import cats.effect._
import cats.implicits._
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.freespec.AsyncFreeSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import sre.api._
import sre.api.finance._
import sre.api.finance.analytics._

class AnalyticsClientSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {

  private def round(n: Double): Float = BigDecimal(n).setScale(0, BigDecimal.RoundingMode.HALF_UP).toFloat

  lazy val settings = Settings.loadFinanceSettings()

  implicit def logger[IO[_]: Sync]: Logger[IO] = Slf4jLogger.getLogger[IO]

  "AnalyticsClient" - {
    "should reindex all ofx files" in {
      DBClient.resource[IO](settings).map { dbClient =>
        val indexClient = AnalyticsIndexClient[IO](dbClient, settings)
        AnalyticsClient[IO](indexClient, dbClient, settings)
      }.use { analyticsClient =>
        for {
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
              println("############> " + period + "\n" + result)
              period.result shouldBe result

            case (period, _) =>
              sys.error(s"Missing period ${period.yearMonth} in database")
          }

          val computedBalancesByYearMonth = completePeriodIndexes.map { periodIndex =>
            val balance = (periodIndex.totalBalance - periodIndex.result)
            periodIndex.yearMonth.minusMonths(1) -> balance
          }.toMap

          val balancesByYearMonth = completePeriodIndexes.map { periodIndex =>
            periodIndex.yearMonth -> periodIndex.totalBalance
          }.toMap

          balancesByYearMonth.toSeq.sortBy(_._1).foreach {
            case (yearMonth, balance) if yearMonth != YearMonth.of(2018, 8)=>
              computedBalancesByYearMonth.get(yearMonth) match {
                case Some(computedBalance) =>
                  round(computedBalance) shouldBe round(balance)

                case None =>
                  println(s"#> Period $yearMonth ignored")
              }

            case _ =>
          }
        }
      }
    }
  }
}
