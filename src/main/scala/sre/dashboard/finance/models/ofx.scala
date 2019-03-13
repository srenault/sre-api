package sre.dashboard.finance

import cats.effect._
import cats.data.OptionT
import cats.implicits._
import java.time.format.{ DateTimeFormatter, DateTimeFormatterBuilder }
import java.time.temporal.ChronoField
import java.time.LocalDate
import java.io.{ File, InputStream, ByteArrayInputStream, FileInputStream }

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
  typ: OfxStrTrnType,
  posted: LocalDate,
  user: LocalDate,
  amount: Float,
  name: String
)

object OfxStmTrn {

  object OfxFile {
    val Reg = """(.+)?\.ofx""".r

    val format = new DateTimeFormatterBuilder()
      .appendPattern("yyyy-MM")
      .parseDefaulting(ChronoField.DAY_OF_MONTH, 1)
      .toFormatter();

    def unapply(file: File): Option[LocalDate] = {
      file.getName match {
        case Reg(dateStr) =>
          scala.util.control.Exception.nonFatalCatch[LocalDate].opt {
            LocalDate.parse(dateStr, format)
          }

        case _ => None
      }
    }
  }

  def load[F[_]: Effect](ofxDirectory: File, date: LocalDate): OptionT[F, List[OfxStmTrn]] = {
    val maybeOfxFile = ofxDirectory.listFiles.find {
        case file@OfxFile(d) => d == date
    }

    maybeOfxFile match {
      case Some(ofxFile) =>
        val is = new FileInputStream(ofxFile)
        OptionT.liftF(load(is))
      case None => OptionT.none
    }
  }

  def load[F[_]: Effect](s: String): F[List[OfxStmTrn]] = {
    val is: InputStream = new ByteArrayInputStream(s.getBytes())
    load(is)
  }

  def load[F[_]](is: InputStream)(implicit F: Effect[F]): F[List[OfxStmTrn]] =
    F.pure {
      import com.webcohesion.ofx4j.io.DefaultHandler
      import com.webcohesion.ofx4j.io.nanoxml.NanoXMLOFXReader
      import scala.collection.mutable.Stack

      val ofxReader = new NanoXMLOFXReader()
      val stack = Stack.empty[List[String]]

      ofxReader.setContentHandler(new DefaultHandler() {

        override def onElement(name: String, value: String) {
          if (List("TRNTYPE", "DTPOSTED", "DTUSER", "TRNAMT", "NAME").exists(_ == name)) {
            val updated = stack.pop() :+ value
            stack.push(updated)
          }
        }

        override def startAggregate(aggregateName: String) {
          if (aggregateName == "STMTTRN") {
            stack.push(Nil)
          }
        }
      })

      ofxReader.parse(is)

      stack.map {
        case typStr :: postedStr :: userStr :: amountStr :: name :: Nil =>
          val typ = OfxStrTrnType(typStr)
          val posted = LocalDate.parse(postedStr, DateTimeFormatter.BASIC_ISO_DATE)
          val user = LocalDate.parse(userStr, DateTimeFormatter.BASIC_ISO_DATE)
          OfxStmTrn(typ, posted, user, amountStr.toFloat, name)

        case x =>
          sys.error(s"Unable to parse OfxStmTrn from $x")
      }.toList
    }
}
