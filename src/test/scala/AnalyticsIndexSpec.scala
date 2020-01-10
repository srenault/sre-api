import java.io._
import org.scalatest._
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import sre.api.finance._

class AnalyticsIndexSpec extends AsyncIOSpec with Matchers {

  val (accountDirs, ofxFiles) = {
    val transactionsDir = new File(getClass.getResource("/transactions").getFile)
    val accountDirs@(accountDir :: _) = transactionsDir.listFiles.toList.filter(_.isDirectory)
    val ofxFiles = ofx.OfxDir.listFiles(accountDir).sortBy(-_.date.toEpochDay).take(2)
    accountDirs -> ofxFiles
  }

  "AnalyticsIndex " - {
    "buildHistoryIndexes" in {
      analytics.AnalyticsIndex.buildHistoryIndexes[IO](accountDirs, ofxFiles)(_ => true).asserting(_ shouldBe Nil)
    }
  }
}
