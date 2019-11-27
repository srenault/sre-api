package sre.api

import java.io.File
import scala.concurrent.duration.FiniteDuration
import cats.effect._
import org.http4s.Uri
import io.circe._
import io.circe.generic.auto._
import io.circe.config.syntax._
import cron4s.expr.CronExpr

case class TrainSettings(endpoint: Uri)

case class TransportSettings(train: TrainSettings)

case class IComptaCategorySettings(label: String, rulePath: List[String], threshold: Int)

case class IComptaSettings(db: String, wageRuleId: String)

case class CMTasksSettings(balances: CronExpr, expenses: CronExpr)

case class CMCacheSettings(size: Int, ttl: FiniteDuration)

case class CMCachesSettings(
  form: CMCacheSettings,
  balances: CMCacheSettings,
  ofx: CMCacheSettings,
  csv: CMCacheSettings
)

case class CMAccountSettings(
  id: String,
  `type`: finance.cm.CMAccountType,
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
  tasks: CMTasksSettings,
  cache: CMCachesSettings,
  otpSession: String,
  apkId: String
) {
  val authenticationUri: Uri = baseUri.withPath(authenticationPath)
  val validationUri: Uri = baseUri.withPath(validationPath)
  val homeUri: Uri = baseUri.withPath(homePath)
  val downloadUri: Uri = baseUri.withPath(downloadPath)
  val transactionUri: Uri = baseUri.withPath(transactionPath)
  def otpSessionFile[F[_]: Sync] = finance.cm.CMOtpSessionFile(otpSession)
}

case class FinanceSettings(icompta: IComptaSettings, cm: CMSettings, transactionsDir: File)

case class DomoticzDeviceSettings(idx: Int)

case class DomoticzSettings(endpoint: Uri, username: String, password: String, teleinfo: DomoticzDeviceSettings)

case class ElectricityRatioSettings(
  hp: Float,
  hc: Float,
  taxeCommunale: Float,
  taxeDepartementale: Float,
  cspe: Float,
  tvaReduite: Float,
  tva: Float,
  cta: Float
)

case class ElectricitySettings(ratio: ElectricityRatioSettings, monthlySubscription: Float, monthlyCta: Float)

case class EnergySettings(electricity: ElectricitySettings)

case class WeatherSettings(endpoint: Uri)

case class S3Settings(bucket: String, publicKey: String, secretKey: String, prefix: Option[String])

case class ApkSettings(s3: S3Settings)

case class Settings(
  advertisedAddress: String,
  httpPort: Int,
  db: String,
  transport: TransportSettings,
  finance: FinanceSettings,
  domoticz: DomoticzSettings,
  energy: EnergySettings,
  weather: WeatherSettings,
  apk: ApkSettings
)

object Settings {

  lazy val AppConfig: com.typesafe.config.Config =
    com.typesafe.config.ConfigFactory.load()

  def load(): Either[Error, Settings] = {
    val advertisedAddress = AppConfig.getString("advertisedAddress")
    val httpPort = AppConfig.getInt("httpPort")
    val db = AppConfig.getString("db")
    for {
      trainSettings <- AppConfig.as[TrainSettings]("transport.train")
      transportSettings = TransportSettings(trainSettings)
      financeSettings <- AppConfig.as[FinanceSettings]("finance")
      domoticzSettings <- AppConfig.as[DomoticzSettings]("domoticz")
      energySettings <- AppConfig.as[EnergySettings]("energy")
      weatherSettings <- AppConfig.as[WeatherSettings]("weather")
      apkSettings <- AppConfig.as[ApkSettings]("apk")
    } yield Settings(advertisedAddress, httpPort, db, transportSettings, financeSettings, domoticzSettings,
      energySettings, weatherSettings, apkSettings)
  }

  implicit val UriDecoder: Decoder[Uri] = new Decoder[Uri] {
    final def apply(c: HCursor): Decoder.Result[Uri] =
      c.as[String].right.flatMap { s =>
        Uri.fromString(s).left.map { error =>
          DecodingFailure(error.message, c.history)
        }
      }
  }

  implicit val FileDecoder: Decoder[File] = new Decoder[File] {
    final def apply(c: HCursor): Decoder.Result[File] =
      c.as[String].right.flatMap { s =>
        val f = new File(s)
        if (f.exists) Right(f) else Left {
          DecodingFailure(s"$s doesn't exists", c.history)
        }
      }
  }

  implicit val CronExprDecoder: Decoder[CronExpr] = new Decoder[CronExpr] {
    final def apply(c: HCursor): Decoder.Result[CronExpr] =
      c.as[String].right.flatMap { s =>
        cron4s.Cron(s).left.map { e =>
          DecodingFailure(s"$s isn't a valid cron expression: $e", c.history)
        }
      }
  }

  implicit val CMAccountTypeDecoder: Decoder[finance.cm.CMAccountType] = new Decoder[finance.cm.CMAccountType] {
    final def apply(c: HCursor): Decoder.Result[finance.cm.CMAccountType] =
      c.as[String].right.flatMap {
        case id if id == finance.cm.CMAccountType.Saving.id =>
          Right(finance.cm.CMAccountType.Saving)

        case id if id == finance.cm.CMAccountType.Current.id =>
          Right(finance.cm.CMAccountType.Current)

        case id if id == finance.cm.CMAccountType.Joint.id =>
          Right(finance.cm.CMAccountType.Joint)

        case id =>
          Left(DecodingFailure(s"Can't parse $id as CMAccountType", c.history))
      }
  }
}
