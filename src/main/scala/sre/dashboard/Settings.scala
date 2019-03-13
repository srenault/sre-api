package sre.dashboard

import java.io.File
import scala.concurrent.duration.FiniteDuration
import org.http4s.Uri
import io.circe._
import io.circe.generic.auto._
import io.circe.config.syntax._
import cron4s.expr.CronExpr

case class TrainSettings(endpoint: Uri)

case class TransportSettings(train: TrainSettings)

case class IComptaSettings(db: String, accountId: String, categories: Map[String, List[String]])

case class CMTasksSettings(balances: CronExpr, expenses: CronExpr)

case class CMCacheSettings(size: Int, ttl: FiniteDuration)

case class CMCachesSettings(
  form: CMCacheSettings,
  balances: CMCacheSettings,
  ofx: CMCacheSettings,
  csv: CMCacheSettings
)

case class CMSettings(
  baseUri: Uri,
  authenticationPath: String,
  downloadPath: String,
  username: String,
  password: String,
  tasks: CMTasksSettings,
  cache: CMCachesSettings
) {
  def authenticationUri: Uri = baseUri.withPath(authenticationPath)
  def downloadUri: Uri = baseUri.withPath(downloadPath)
}

case class FinanceSettings(icompta: IComptaSettings, cm: CMSettings)

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

case class Settings(
  httpPort: Int,
  db: String,
  transport: TransportSettings,
  finance: FinanceSettings,
  domoticz: DomoticzSettings,
  energy: EnergySettings,
  weather: WeatherSettings
)

object Settings {

  val CONFIG_FILE_NAME = "app.conf"

  lazy val AppConfig: com.typesafe.config.Config =
    com.typesafe.config.ConfigFactory.parseResources(CONFIG_FILE_NAME)

  def load(): Either[Error, Settings] = {
    val httpPort = AppConfig.getInt("httpPort")
    val db = AppConfig.getString("db")
    for {
      trainSettings <- AppConfig.as[TrainSettings]("transport.train")
      transportSettings = TransportSettings(trainSettings)
      financeSettings <- AppConfig.as[FinanceSettings]("finance")
      domoticzSettings <- AppConfig.as[DomoticzSettings]("domoticz")
      energySettings <- AppConfig.as[EnergySettings]("energy")
      weatherSettings <- AppConfig.as[WeatherSettings]("weather")
    } yield Settings(httpPort, db, transportSettings, financeSettings, domoticzSettings,
      energySettings, weatherSettings)
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
}
