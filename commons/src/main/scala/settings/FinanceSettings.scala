package sre.api
package settings

import java.io.File
import java.nio.file.Path
import scala.concurrent.duration.FiniteDuration
import cats.effect._
import cats.implicits._
import org.http4s.Uri
import io.circe._
import io.circe.generic.semiauto._
import JsonImplicits._


case class CMAccountSettings(
  id: String,
  `type`: String,
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
  otpSession: Path,
  apkId: String
) {
  val authenticationUri: Uri = baseUri.withPath(authenticationPath)
  val validationUri: Uri = baseUri.withPath(validationPath)
  val homeUri: Uri = baseUri.withPath(homePath)
  val downloadUri: Uri = baseUri.withPath(downloadPath)
  val transactionUri: Uri = baseUri.withPath(transactionPath)
}

object CMSettings {

  implicit val decoder: Decoder[CMSettings] = deriveDecoder[CMSettings]
}

case class SetupVolumeSettings(maxKeys: Int)

object SetupVolumeSettings {

  implicit val decoder: Decoder[SetupVolumeSettings] = deriveDecoder[SetupVolumeSettings]
}

case class FinanceSettings(
  db: String,
  httpClient: HttpClientSettings,
  cm: CMSettings,
  transactionsDir: Path,
  wageStatements: List[String],
  s3: S3Settings,
  setupVolume: SetupVolumeSettings
) {
  def accountsDir: List[File] = transactionsDir.toFile.listFiles.toList.filter(_.isDirectory)
}

object FinanceSettings {

  def fromEnv[F[_]]()(implicit F: Sync[F]): F[FinanceSettings] = F.pure {
    FinanceSettings(
      db = Env.getStringOrFail("DB_PATH"),
      httpClient = HttpClientSettings(
        logRequest = Env.getBooleanOrFail("HTTPCLIENT_LOGREQUEST"),
        logResponse = Env.getBooleanOrFail("HTTPCLIENT_LOGRESPONSE")
      ),
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
        otpSession = Env.getPathOrFail("FINANCE_CM_OTPSESSION"),
        apkId = Env.getStringOrFail("FINANCE_CM_APKID")
      ),
      wageStatements = Env.getJsonAsOrFail[List[String]]("FINANCE_WAGE_STATEMENTS"),
      transactionsDir = Env.getPathOrFail("FINANCE_TRANSACTIONS_DIR"),
      s3 = S3Settings(
        region = Env.getStringOrFail("FINANCE_S3_TRANSACTIONS_REGION"),
        bucket = Env.getStringOrFail("FINANCE_S3_TRANSACTIONS_BUCKET"),
        publicKey = Env.getStringOrFail("FINANCE_S3_TRANSACTIONS_PUBLICKEY"),
        secretKey = Env.getStringOrFail("FINANCE_S3_TRANSACTIONS_SECRETKEY"),
        prefix = Env.getString("FINANCE_S3_TRANSACTIONS_PREFIX").getOrElse("")
      ),
      setupVolume = SetupVolumeSettings(maxKeys = Env.getIntOrFail("FINANCE_SETUP_VOLUME_MAXKEYS"))
    )
  }

  implicit val decoder: Decoder[FinanceSettings] = deriveDecoder[FinanceSettings]
}
