package sre.api.finance.cm

import scala.jdk.CollectionConverters._
import cats.implicits._

case class CMAccountInput(index: Int, id: String, label: String, checkId: String, checkName: String)

object CMAccountInput {

  def parse(doc: org.jsoup.nodes.Document): Either[String, List[CMAccountInput]] = {
    doc.select("#account-table label").asScala.zipWithIndex.map {
      case (domLabel, index) =>
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
          checkId <- checkIdOrError
          check <- checkOrError(checkId)
          checkName <- checkNameOrError(check)
        } yield CMAccountInput(index, id, label, checkId, checkName)

    }.toList.sequence
  }

  def parseOrFail(doc: org.jsoup.nodes.Document): List[CMAccountInput] =
    parse(doc) match {
      case Left(error) => sys.error(s"Unable to parse $doc as CMAccountInput: $error")
      case Right(inputs) => inputs
    }
}
