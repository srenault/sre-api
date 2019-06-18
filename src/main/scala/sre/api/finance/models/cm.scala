package sre.api.finance

import java.time.LocalDate
import scala.collection.JavaConverters._
import cats.implicits._
import cats.effect._
import org.http4s._
import org.http4s.EntityEncoder
import org.http4s.headers._
import org.http4s.circe._
import io.circe.{ Json, Encoder }
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

case class CMAccount(
  id: String,
  `type`: CMAccountType,
  label: String,
  displayName: Option[String],
  balance: Float,
  statements: List[CMStatement]
) {
  def since(date: LocalDate): CMAccount = {
    this.copy(statements = statements.filter { statement =>
      statement.date.equals(date) || statement.date.isAfter(date)
    })
  }
}

sealed trait CMAccountType {
  def id: String
}

object CMAccountType {

  implicit val encoder: Encoder[CMAccountType] = new Encoder[CMAccountType] {
    final def apply(a: CMAccountType): Json = Json.fromString(a.id)
  }

  case object Saving extends CMAccountType {
    def id = "saving_account"
  }

  case object Current extends CMAccountType {
    def id = "current_account"
  }

  case object Joint extends CMAccountType {
    def id = "joint_account"
  }

  case object Unknown extends CMAccountType {
    def id = "n/a"
  }
}

object CMAccount {
  implicit val encoder: Encoder[CMAccount] = deriveEncoder[CMAccount]
  implicit def entityEncoder[F[_]: Effect]: EntityEncoder[F, CMAccount] = jsonEncoderOf[F, CMAccount]
  implicit def entitiesEncoder[F[_]: Effect]: EntityEncoder[F, List[CMAccount]] = jsonEncoderOf[F, List[CMAccount]]
}

case class CMStatement(
  date: LocalDate,
  amount: Float,
  label: String,
  balance: Float
)

object CMStatement {
  implicit val encoder: Encoder[CMStatement] = deriveEncoder[CMStatement]
  implicit def entityEncoder[F[_]: Effect]: EntityEncoder[F, CMStatement] = jsonEncoderOf[F, CMStatement]
  implicit def entitiesEncoder[F[_]: Effect]: EntityEncoder[F, List[CMStatement]] = jsonEncoderOf[F, List[CMStatement]]

  def computeCreditAndDebit(statements: List[CMStatement]): (Float, Float) = {
    val (credit, debit) = statements.map(_.amount).partition(_ > 0)
    credit.foldLeft(0F)(_ + _) -> debit.foldLeft(0F)(_ + _)
  }
}

case class CMCsvRecord(
  date: String,
  dateValue: String,
  amount: String,
  label: String,
  balance: String
) {
  def toStatement: CMStatement =
    CMCsvRecord.toStatement(this)
}

object CMCsvRecord {
  import java.time.format.DateTimeFormatterBuilder

  private val format = new DateTimeFormatterBuilder()
    .appendPattern("dd/MM/yyyy")
    .toFormatter();

  private def parseDateOrFail(s: String): LocalDate =
    LocalDate.parse(s, format)

  def toStatement(csvRecord: CMCsvRecord): CMStatement = {
    val date = parseDateOrFail(csvRecord.date)
    CMStatement(date, csvRecord.amount.toFloat, csvRecord.label, csvRecord.balance.toFloat)
  }
}

object CMCsvLine {

  def parseOrFail(line: String): CMCsvRecord = {
    line.split(";").toList match {
      case date :: valueDate :: amount :: label :: balance :: Nil =>
        CMCsvRecord(date, valueDate, amount, label, balance)

      case _ =>
        sys.error(s"Unable to parse $line as CMCsvLine")
    }
  }
}

case class CMPeriod(startDate: LocalDate, endDate: LocalDate, statements: List[CMStatement])
