package sre.api

import java.io.File
import org.http4s.Uri
import io.circe._
import io.circe.generic.auto._
import io.circe.config.syntax._
import sre.api.domoticz.DomoticzSettings

case class TrainSettings(endpoint: Uri)

case class TransportSettings(train: TrainSettings)

case class IComptaCategorySettings(
    label: String,
    rulePath: List[String],
    threshold: Int
)

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

case class ElectricitySettings(
    ratio: ElectricityRatioSettings,
    yearlySubscription: Float,
    monthlyCta: Float
)

case class EnergySettings(electricity: ElectricitySettings)

case class WeatherSettings(endpoint: Uri)

case class S3Settings(
    region: String,
    bucket: String,
    publicKey: String,
    secretKey: String,
    prefix: Option[String]
)

case class HttpClientSettings(
    logRequest: Boolean,
    logResponse: Boolean
)

case class Settings(
    advertisedAddress: String,
    httpPort: Int,
    httpClient: HttpClientSettings,
    db: String,
    cors: Boolean,
    transport: TransportSettings,
    domoticz: DomoticzSettings,
    energy: EnergySettings,
    weather: WeatherSettings
)

object Settings {

  lazy val AppConfig: com.typesafe.config.Config =
    com.typesafe.config.ConfigFactory.load()

  def load(): Either[Error, Settings] = {
    val advertisedAddress = AppConfig.getString("advertisedAddress")
    val httpPort = AppConfig.getInt("httpPort")
    val db = AppConfig.getString("db")
    val cors = AppConfig.getBoolean("cors")
    for {
      httpClientSettings <- AppConfig.as[HttpClientSettings]("httpClient")
      trainSettings <- AppConfig.as[TrainSettings]("transport.train")
      transportSettings = TransportSettings(trainSettings)
      domoticzSettings <- AppConfig.as[DomoticzSettings]("domoticz")
      energySettings <- AppConfig.as[EnergySettings]("energy")
      weatherSettings <- AppConfig.as[WeatherSettings]("weather")
    } yield Settings(
      advertisedAddress,
      httpPort,
      httpClientSettings,
      db,
      cors,
      transportSettings,
      domoticzSettings,
      energySettings,
      weatherSettings
    )
  }

  implicit val UriDecoder: Decoder[Uri] = new Decoder[Uri] {
    final def apply(c: HCursor): Decoder.Result[Uri] =
      c.as[String].flatMap { s =>
        Uri.fromString(s).left.map { error =>
          DecodingFailure(error.message, c.history)
        }
      }
  }

  implicit val FileDecoder: Decoder[File] = new Decoder[File] {
    final def apply(c: HCursor): Decoder.Result[File] =
      c.as[String].flatMap { s =>
        val f = new File(s)
        if (f.exists) Right(f)
        else
          Left {
            DecodingFailure(s"$s file doesn't exists", c.history)
          }
      }
  }
}
