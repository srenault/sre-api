package sre.api.finance

import java.io.File
import scala.concurrent.duration.FiniteDuration
import cats.effect._
import org.http4s.Uri

case class IComptaCategorySettings(label: String, rulePath: List[String], threshold: Int)

case class IComptaSettings(db: String, wageRuleId: String)

case class CMAccountSettings(
  id: String,
  `type`: cm.CMAccountType,
  label: String,
  categories: Map[String, IComptaCategorySettings]
)

case class CMSettings(
  baseUri: Uri,
  authenticationPath: String,
  validationPath: String,
  homePath: String,
  downloadPath: String,
  transactionPath: String,
  username: String,
  password: String,
  accounts: List[CMAccountSettings],
  otpSession: String,
  apkId: String
) {
  val authenticationUri: Uri = baseUri.withPath(authenticationPath)
  val validationUri: Uri = baseUri.withPath(validationPath)
  val homeUri: Uri = baseUri.withPath(homePath)
  val downloadUri: Uri = baseUri.withPath(downloadPath)
  val transactionUri: Uri = baseUri.withPath(transactionPath)
  def getOtpSessionFile[F[_]: Sync] = cm.CMOtpSessionFile(otpSession)
}

case class FinanceSettings(icompta: IComptaSettings, cm: CMSettings, transactionsDir: File) {
  def accountsDir: List[File] = transactionsDir.listFiles.toList.filter(_.isDirectory)
}

case class HttpClientSettings(
  logRequest: Boolean,
  logResponse: Boolean
)

case class Settings(
  httpClient: HttpClientSettings,
  db: String,
  finance: FinanceSettings
)

object Settings {

  lazy val env = System.getenv

  private def getString(key: String): Option[String] =
    Option(env.get(key))

  private def getStringOrFail(key: String): String =
    getString(key) getOrElse sys.error(s"Configuration error: Unable to get $key")

  private def getUri(key: String): Option[Uri] =
    getString(key).flatMap(Uri.fromString(_).toOption)

  private def getUriOrFail(key: String): Uri =
    getUri(key) getOrElse sys.error(s"Configuration error: Unable to get $key")

  private def getBoolean(key: String): Option[Boolean] =
    scala.util.Try(getStringOrFail(key).toBoolean).toOption

  private def getBooleanOrFail(key: String): Boolean =
    getBoolean(key) getOrElse {
      sys.error(s"Configuration error: Unable to get $key as Boolean")
    }


  def build(): Settings = {
    ???
  }
}