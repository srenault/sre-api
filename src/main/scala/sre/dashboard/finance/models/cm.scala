package sre.dashboard.finance

import java.time.LocalDate
import scala.collection.JavaConverters._
import cats.implicits._
import cats.effect._
import org.http4s._
import org.http4s.EntityEncoder
import org.http4s.headers._
import org.http4s.circe._
import io.circe.Encoder
import io.circe.generic.semiauto._

case class CMSession(cookie: ResponseCookie) {
  def toRequestCookie: Cookie = {
    Cookie(RequestCookie(cookie.name, cookie.content))
  }
}

case class CMDownloadForm(action: String, inputs: List[CMAccountInput])

object CMDownloadForm {

  def parse(html: String): Either[String, CMDownloadForm] = {
    Either.catchNonFatal {
      val doc = org.jsoup.Jsoup.parse(html)
      parse(doc)
    }.left.map(_.getMessage).flatten
  }

  def parse(doc: org.jsoup.nodes.Document): Either[String, CMDownloadForm] = {

    val formOrError: Either[String, org.jsoup.nodes.Element] =
      doc.select("""[id="P:F"]""").asScala.headOption match {
        case Some(el) => Right(el)
        case None => Left("Unable to get download form")
      }

    val actionOrError: org.jsoup.nodes.Element => Either[String, String] =
      (form) => {
        Option(form.attributes.get("action")) match {
          case Some(action) => Right(action)
          case None => Left("Unable to get action")
        }
      }

    for {
      form <- formOrError.right
      action <- actionOrError(form).right
      inputs <- CMAccountInput.parse(doc).right
    } yield CMDownloadForm(action, inputs)
  }

  def parseOrFail(html: String): CMDownloadForm = {
    val doc = org.jsoup.Jsoup.parse(html)
    parseOrFail(doc)
  }

  def parseOrFail(doc: org.jsoup.nodes.Document): CMDownloadForm = {
    parse(doc) match {
      case Left(error) => sys.error(s"Unable to parse cm form: $doc")
      case Right(form) => form
    }
  }
}

case class CMAccountInput(id: String, label: String, checkId: String, checkName: String)

object CMAccountInput {

  def parse(doc: org.jsoup.nodes.Document): Either[String, List[CMAccountInput]] = {
    doc.select("#account-table label").asScala.map { domLabel =>
      val label = domLabel.text
      val id = label.split(" ").take(3).mkString("")

      val checkIdOrError = Option(domLabel.attributes.get("for")) match {
        case Some(checkId) => Right(checkId)
        case None => Left("Unable to get checkId")
      }

      val checkOrError = (checkId: String) => {
        doc.select(s"""[id="$checkId"]""").asScala.headOption match {
          case Some(checkName) => Right(checkName)
          case None => Left(s"Unable to get check $checkId")
        }
      }

      val checkNameOrError = (check: org.jsoup.nodes.Element) => {
        Option(check.attributes.get("name")) match {
          case Some(checkName) => Right(checkName)
          case None => Left("Unable to get checkName")
        }
      }

      for {
        checkId <- checkIdOrError.right
        check <- checkOrError(checkId).right
        checkName <- checkNameOrError(check).right
      } yield CMAccountInput(id, label, checkId, checkName)
    }.toList.sequence
  }

  def parseOrFail(doc: org.jsoup.nodes.Document): List[CMAccountInput] =
    parse(doc) match {
      case Left(error) => sys.error(s"Unable to parse $doc as CMAccountInput: $error")
      case Right(inputs) => inputs
    }
}

case class CMAccount(id: String, label: String, balance: Float)

object CMAccount {

  def apply(input: CMAccountInput, balance: Float): CMAccount =
    CMAccount(input.id, input.label, balance)

  implicit val encoder: Encoder[CMAccount] = deriveEncoder[CMAccount]
  implicit def entityEncoder[F[_]: Effect]: EntityEncoder[F, CMAccount] = jsonEncoderOf[F, CMAccount]
  implicit def entitiesEncoder[F[_]: Effect]: EntityEncoder[F, List[CMAccount]] = jsonEncoderOf[F, List[CMAccount]]
}

case class CMCsvRecord(
  date: LocalDate,
  dateValue: LocalDate,
  amount: Float,
  label: String,
  balance: Float
)

object CMcsvLine {
  import java.time.format.DateTimeFormatterBuilder

  private val format = new DateTimeFormatterBuilder()
    .appendPattern("dd/MM/yyyy")
    .toFormatter();

  private def parseDateOrFail(s: String): LocalDate =
    LocalDate.parse(s, format)

  def parseOrFail(line: String): CMCsvRecord = {
    line.split(";").toList match {
      case dateStr :: valueDateStr :: amount :: label :: balance :: Nil =>
        val date = parseDateOrFail(dateStr)
        val valueDate = parseDateOrFail(valueDateStr)
        CMCsvRecord(date, valueDate, amount.toFloat, label, balance.toFloat)

      case _ =>
        sys.error(s"Unable to parse $line as CMCsvLine")
    }
  }
}
