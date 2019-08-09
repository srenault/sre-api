package sre.api.finance

import cats.effect._
import java.time.format.DateTimeFormatter
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
  `type`: OfxStrTrnType,
  posted: LocalDate,
  user: LocalDate,
  amount: Float,
  name: String
) {
  def toStatement: CMStatement = {
    CMStatement(posted, amount, name, None)
  }
}

case class OfxFile(file: File, date: LocalDate) {
  lazy val name = file.getName
}

object OfxFile {
  val Reg = """(.+)?\.ofx""".r

  val format = DateTimeFormatter.ofPattern("yyyy-MM-dd")

  def unapply(file: File): Option[OfxFile] = {
    file.getName match {
      case Reg(dateStr) =>
        scala.util.control.Exception.nonFatalCatch[OfxFile].opt {
          val date = LocalDate.parse(dateStr, format)
          OfxFile(file, date)
        }

      case _ => None
    }
  }
}

object OfxDir {

  def listFiles(dir: File): List[OfxFile] = {
    dir.listFiles.toList.collect {
      case OfxFile(ofxFile) => ofxFile
    }
  }
}

object OfxStmTrn {

  // def streamOfxDir[F[_]](ofxDir: File)(implicit F: Effect[F]): Stream[F,List[OfxStmTrn]] = {
  //   val sortedFiles = ofxDir.listFiles.collect {
  //     case file@OfxFile(date) => file -> date
  //   }.sortBy { case (_, date) => -date.toEpochDay }
  //     .map(_._1)

  //   Stream.emits(sortedFiles).flatMap { file =>
  //     Stream.eval(load(file))
  //   }
  // }

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
          val `type` = OfxStrTrnType(typStr)
          val posted = LocalDate.parse(postedStr, DateTimeFormatter.BASIC_ISO_DATE)
          val user = LocalDate.parse(userStr, DateTimeFormatter.BASIC_ISO_DATE)
          OfxStmTrn(`type`, posted, user, amountStr.toFloat, name)

        case x =>
          sys.error(s"Unable to parse OfxStmTrn from $x")
      }.toList
    }

  def load[F[_]: Effect](ofxFile: File): F[List[OfxStmTrn]] = {
    val is = new FileInputStream(ofxFile)
    load(is)
  }

  def load[F[_]: Effect](s: String): F[List[OfxStmTrn]] = {
    val is: InputStream = new ByteArrayInputStream(s.getBytes())
    load(is)
  }
}
