package sre.api.finance.cm

import scala.jdk.CollectionConverters._
import cats.implicits._

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
      doc.select("""[id="P1:F"]""").asScala.headOption match {
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
      form <- formOrError
      action <- actionOrError(form)
      inputs <- CMAccountInput.parse(doc)
    } yield CMDownloadForm(action, inputs)
  }

  def parseOrFail(html: String): CMDownloadForm = {
    val doc = org.jsoup.Jsoup.parse(html)
    parseOrFail(doc)
  }

  def parseOrFail(doc: org.jsoup.nodes.Document): CMDownloadForm = {
    parse(doc) match {
      case Left(error) => sys.error(s"Unable to parse cm form: $error:\n$doc")
      case Right(form) => form
    }
  }
}
