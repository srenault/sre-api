package sre.api
package finance

import java.io.File
import scala.concurrent.duration.FiniteDuration
import cats.effect._
import org.http4s.Uri
import io.circe._
import io.circe.generic.semiauto._

case class CMAccountSettings(
  id: String,
  `type`: cm.CMAccountType,
  label: String
)

object CMAccountSettings {

  implicit val decoder: Decoder[CMAccountSettings] = deriveDecoder[CMAccountSettings]
}

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

case class FinanceSettings(cm: CMSettings, transactionsDir: File, wageStatements: List[String]) {
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

  def build(): Settings = {
    Settings(
      httpClient = HttpClientSettings(
        logRequest = Env.getBooleanOrFail("HTTPCLIENT_LOGREQUEST"),
        logResponse = Env.getBooleanOrFail("HTTPCLIENT_LOGRESPONSE")
      ),
      db = Env.getStringOrFail("DB_PATH"),
      finance = FinanceSettings(
        cm = CMSettings(
          baseUri = Env.getUriOrFail("FINANCE_CM_BASE_URI"),
          authenticationPath = Env.getStringOrFail("FINANCE_CM_AUTHENTICATION_PATH"),
          validationPath = Env.getStringOrFail("FINANCE_CM_VALIDATION_PATH"),
          homePath = Env.getStringOrFail("FINANCE_CM_HOME_PATH"),
          downloadPath = Env.getStringOrFail("FINANCE_CM_DOWNLOAD_PATH"),
          transactionPath = Env.getStringOrFail("FINANCE_CM_TRANSACTION_PATH"),
          username = Env.getStringOrFail("FINANCE_CM_USERNAME"),
          password = Env.getStringOrFail("FINANCE_CM_PASSWORD"),
          accounts = Env.getJsonAsOrFail[List[CMAccountSettings]]("FINANCE_CM_ACCOUNTS"),
          otpSession = Env.getStringOrFail("FINANCE_CM_OTPSESSION"),
          apkId = Env.getStringOrFail("FINANCE_CM_APKID")
        ),
        wageStatements = Env.getJsonAsOrFail[List[String]]("FINANCE_WAGE_STATEMENTS"),
        transactionsDir = Env.getFileOrFail("FINANCE_TRANSACTIONS_DIR")
      )
    )
  }
}
