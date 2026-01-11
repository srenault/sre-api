package sre.api.finance.cm

import scala.jdk.CollectionConverters._
import cats.implicits._

case class CMDownloadForm(action: String, inputs: List[CMAccountInput], cpt: String, wxf2cc: String)

object CMDownloadForm {

  def parse(html: String): Either[String, CMDownloadForm] = {
    Either.catchNonFatal {
      val doc = org.jsoup.Jsoup.parse(html)
      parse(doc)
    }.left.map(_.getMessage).flatten
  }

  def parse(doc: org.jsoup.nodes.Document): Either[String, CMDownloadForm] = {

    val formOrError: Either[String, org.jsoup.nodes.Element] =
      doc.select("""[id="C:P:F"]""").asScala.headOption match {
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

    val cptOrError: Either[String, String] =
      Option(doc.select("""input[name="$CPT"]""").attr("value")).filter(_.nonEmpty) match {
        case Some(cpt) => Right(cpt)
        case None => Left("Unable to get $CPT")
      }

    val wxf2ccOrError: Either[String, String] =
      Option(doc.select("""input[name="_wxf2_cc"]""").attr("value")).filter(_.nonEmpty) match {
        case Some(wxf2cc) => Right(wxf2cc)
        case None => Left("Unable to get _wxf2_cc")
      }

    for {
      form <- formOrError
      action <- actionOrError(form)
      inputs <- CMAccountInput.parse(doc)
      cpt <- cptOrError
      wxf2cc <- wxf2ccOrError
    } yield CMDownloadForm(action, inputs, cpt, wxf2cc)
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
