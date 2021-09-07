// import java.io.File
// import org.scalatest.matchers.should.Matchers
// import cats.effect._
// import cats.effect.testing.scalatest.AsyncIOSpec
// import org.scalatest.freespec.AsyncFreeSpec
// import sre.api.finance.ofx._
// import sre.api.finance.cm._

// class OfxSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {

//   "OfxSpec" - {
//     "should compute the right balance for each statement" in {
//       val fileA = new File("src/test/resources/transactions/3719100010480201/2018-07-16.ofx")
//       val fileB = new File("src/test/resources/transactions/3719100010480201/2018-06-14.ofx")
//       val ofxA = OfxFile.fromFile(fileA).get
//       val ofxB = OfxFile.fromFile(fileB).get
//       for {
//         transactionsA <- OfxStmTrn.load[IO](ofxA)
//         transactionsB <- OfxStmTrn.load[IO](ofxB)
//       } yield {
//         println("## transactionsA")
//         transactionsA.foreach(println)
//         println("## transactionsB")
//         transactionsA.map(_.toStatement("3719100010480201")).sorted(CMStatement.ORDER_ASC).foreach(println)
//         println("## transactionsB")
//         transactionsB.foreach(println)
//         true shouldBe true
//       }
//     }
//   }
// }
