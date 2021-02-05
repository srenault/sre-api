package sre.api.finance
package ofx

import cats.effect._
import java.time.format.DateTimeFormatter
import java.time.LocalDate
import java.io.{ File, FileInputStream }
import cm.CMStatement

sealed trait OfxStrTrnType {
  def value: String
}

object OfxStrTrnType {

  def apply(s: String): OfxStrTrnType =
    if (s == "DEBIT") {
      Debit
    } else if (s == "CREDIT") {
      Credit
    } else {
      sys.error(s"Unable to parse TRNTYPE value for $s")
    }


  case object Debit extends OfxStrTrnType {
    def value = "DEBIT"
  }

  case object Credit extends OfxStrTrnType {
    def value = "CREDIT"
  }
}

case class OfxStmTrn(
  fitid: String,
  `type`: OfxStrTrnType,
  posted: LocalDate,
  user: LocalDate,
  amount: Float,
  name: String,
  balance: Float,
  accurateBalance: Boolean,
  downloadedAt: LocalDate,
  pos: Int
) {
  def toStatement(accountId: String): CMStatement = {
    CMStatement(fitid, accountId, posted, amount, name, balance, downloadedAt, pos, accurateBalance)
  }

  def toStatement(ofxFile: OfxFile): CMStatement = {
    CMStatement(
      fitid,
      ofxFile.file.getParentFile.getName,
      posted,
      amount,
      name,
      balance,
      downloadedAt,
      pos,
      accurateBalance
    )
  }
}

case class OfxFile(file: File, date: LocalDate) {
  lazy val name = file.getName
  lazy val accountId = file.getParentFile.getName
}

object OfxFile {
  val Reg = """(.+)?\.ofx""".r

  val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")

  def open(path: String): Option[OfxFile] = {
    val file = new File(path)
    fromFile(file)
  }

  def filename(date: LocalDate): String =
    formatter.format(date) + ".ofx"

  def fromFile(file: File): Option[OfxFile] = {
    file.getName match {
      case Reg(dateStr) =>
        scala.util.control.Exception.nonFatalCatch[OfxFile].opt {
          val date = LocalDate.parse(dateStr, formatter)
          OfxFile(file, date)
        }

      case _ => None
    }
  }
}

object OfxDir {

  def listFiles(dir: File): List[OfxFile] = {
    dir.listFiles.toList.flatMap(OfxFile.fromFile)
  }
}

object OfxStmTrn {

  def load[F[_]](ofxFile: OfxFile)(implicit F: Effect[F]): F[List[OfxStmTrn]] = {
    val is = new FileInputStream(ofxFile.file)

    F.pure {
      import com.webcohesion.ofx4j.io.DefaultHandler
      import com.webcohesion.ofx4j.io.nanoxml.NanoXMLOFXReader
      import scala.collection.mutable.Stack

      val ofxReader = new NanoXMLOFXReader()
      val stackStatements = Stack.empty[List[String]]
      var maybeEndPeriodBalance: Option[Float] = None

      ofxReader.setContentHandler(new DefaultHandler() {

        override def onElement(name: String, value: String): Unit = {
          if (List("TRNTYPE", "DTPOSTED", "DTUSER", "TRNAMT", "FITID", "NAME").exists(_ == name)) {
            val updated = stackStatements.pop() :+ value
            stackStatements.push(updated)
          }

          if (name == "BALAMT" && maybeEndPeriodBalance.isEmpty) {
            maybeEndPeriodBalance = Some(value.toFloat)
          }
        }

        override def startAggregate(aggregateName: String): Unit = {
          if (aggregateName == "STMTTRN") {
            stackStatements.push(Nil)
          }
        }
      })

      ofxReader.parse(is)

      val endPeriodBalance = maybeEndPeriodBalance getOrElse {
        sys.error("Unable to get end period balance")
      }

      stackStatements.toList.zipWithIndex.foldLeft[List[OfxStmTrn]](Nil) {
        case (acc, (typStr :: postedStr :: userStr :: amountStr :: fitid :: name :: Nil, index)) =>
          val amount = amountStr.toFloat
          val `type` = OfxStrTrnType(typStr)
          val posted = LocalDate.parse(postedStr, DateTimeFormatter.BASIC_ISO_DATE)
          val user = LocalDate.parse(userStr, DateTimeFormatter.BASIC_ISO_DATE)
          val balance = acc match {
            case (previousStatement :: _) =>
              previousStatement.balance - previousStatement.amount

            case Nil => endPeriodBalance
          }
          val accurateBalance = acc.size != 0 // The last one is not accurate
          val trn = OfxStmTrn(
            fitid,
            `type`,
            posted,
            user,
            amount,
            name,
            balance,
            accurateBalance,
            downloadedAt = ofxFile.date,
            pos = stackStatements.size - 1 - index
          )

          trn :: acc

        case x =>
          sys.error(s"Unable to parse OfxStmTrn from $x")
      }
    }
  }

  def persist[F[_]: Effect : ContextShift](is: fs2.Stream[F, Byte], accountPath: java.nio.file.Path): F[Unit] = {
    fs2.Stream.resource(Blocker[F]).flatMap { blocker =>
      val filename = OfxFile.filename(LocalDate.now)
      val path = accountPath.resolve(filename)

      java.nio.file.Files.deleteIfExists(path)

      is.through(fs2.io.file.writeAll(path, blocker, java.nio.file.StandardOpenOption.CREATE_NEW :: Nil))

    }.compile.drain
  }
}
