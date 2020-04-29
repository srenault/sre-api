package sre.api.finance
package ofx

import cats.effect._
import java.time.format.DateTimeFormatter
import java.time.LocalDate
import java.io.{ File, InputStream, ByteArrayInputStream, FileInputStream }
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
  name: String
) {
  def toStatement(accountId: String): CMStatement = {
    CMStatement(fitid, accountId, posted, amount, name, None)
  }

  def toStatement(ofxFile: OfxFile): CMStatement = {
    CMStatement(fitid, ofxFile.file.getParentFile.getName, posted, amount, name, None)
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

  def load[F[_]](is: InputStream)(implicit F: Effect[F]): F[List[OfxStmTrn]] =
    F.pure {
      import com.webcohesion.ofx4j.io.DefaultHandler
      import com.webcohesion.ofx4j.io.nanoxml.NanoXMLOFXReader
      import scala.collection.mutable.Stack

      val ofxReader = new NanoXMLOFXReader()
      val stack = Stack.empty[List[String]]

      ofxReader.setContentHandler(new DefaultHandler() {

        override def onElement(name: String, value: String): Unit = {
          if (List("TRNTYPE", "DTPOSTED", "DTUSER", "TRNAMT", "FITID", "NAME").exists(_ == name)) {
            val updated = stack.pop() :+ value
            stack.push(updated)
          }
        }

        override def startAggregate(aggregateName: String): Unit = {
          if (aggregateName == "STMTTRN") {
            stack.push(Nil)
          }
        }
      })

      ofxReader.parse(is)

      stack.map {
        case typStr :: postedStr :: userStr :: amountStr :: fitid :: name :: Nil =>
          val `type` = OfxStrTrnType(typStr)
          val posted = LocalDate.parse(postedStr, DateTimeFormatter.BASIC_ISO_DATE)
          val user = LocalDate.parse(userStr, DateTimeFormatter.BASIC_ISO_DATE)
          OfxStmTrn(fitid, `type`, posted, user, amountStr.toFloat, name)

        case x =>
          sys.error(s"Unable to parse OfxStmTrn from $x")
      }.toList
    }

  def load[F[_]: Effect](file: File): F[List[OfxStmTrn]] = {
    val is = new FileInputStream(file)
    load(is)
  }

  def load[F[_]: Effect](ofxFile: OfxFile): F[List[OfxStmTrn]] = {
    load(ofxFile.file)
  }

  def load[F[_]: Effect](s: String): F[List[OfxStmTrn]] = {
    val is: InputStream = new ByteArrayInputStream(s.getBytes())
    load(is)
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
