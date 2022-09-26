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

case class Settings(
  finance: FinanceSettings,
  heaters: HeatersSettings
)

object Settings {

  lazy val AppConfig: com.typesafe.config.Config =
    com.typesafe.config.ConfigFactory.load()

  def load(maybeConfigPath: Option[Path]): Either[Error, Settings] = {
    com.typesafe.config.ConfigFactory.load()
    for {
      financeSettings <- AppConfig.as[FinanceSettings]("finance")
      heatersSettings <- AppConfig.as[HeatersSettings]("heaters")
    } yield Settings(
      financeSettings,
      heatersSettings
    )
  }
}
