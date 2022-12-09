package sre.cli

import java.io.File
import java.nio.file.Path
import scala.concurrent.duration.FiniteDuration
import cats.effect._
import org.http4s.Uri
import io.circe._
import io.circe.generic.semiauto._
import io.circe.config.syntax._
import sre.api.settings._
import sre.api.domoticz.DomoticzSettings

case class Settings(
  finance: FinanceSettings,
  heaters: HeatersSettings,
  shutters: ShuttersSettings,
  domoticz: DomoticzSettings
)

object Settings {

  lazy val AppConfig: com.typesafe.config.Config =
    com.typesafe.config.ConfigFactory.load()

  def load(maybeConfigPath: Option[Path]): Either[Error, Settings] = {
    com.typesafe.config.ConfigFactory.load()
    for {
      financeSettings <- AppConfig.as[FinanceSettings]("finance")
      heatersSettings <- AppConfig.as[HeatersSettings]("heaters")
      shuttersSettings <- AppConfig.as[ShuttersSettings]("shutters")
      domoticzSettings <- AppConfig.as[DomoticzSettings]("domoticz")
    } yield Settings(
      financeSettings,
      heatersSettings,
      shuttersSettings,
      domoticzSettings
    )
  }
}
