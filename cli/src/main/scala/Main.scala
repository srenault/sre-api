package sre.cli

import cats.effect._
import cats.implicits._
import cats.data.Validated
import com.monovore.decline._
import com.monovore.decline.effect._
import java.nio.file.Path
import org.http4s.ember.client.EmberClientBuilder
import sre.api.settings._
import sre.api.heaters._

sealed trait SreCmd

object Cli extends CommandIOApp(
  name = "sre",
  header = "sreapi command line",
  version = "0.0.1"
) {

  def main: Opts[IO[ExitCode]] = {
    val configOpt = Opts.option[Path]("config", help = "Local path to configuration file")

    //    configOpt.orNone.map { maybeConfigPath =>
    Settings.load(maybeConfigPath = None) match {
      case Right(settings) =>
        val financeOpt: Opts[SreCmd] = Opts.subcommand(FinanceCmd.cmd)
        val heaterOpt: Opts[SreCmd] = Opts.subcommand(HeatersCmd.cmd)

        (financeOpt orElse heaterOpt).map {
          case HeatersCmd(action) =>
            EmberClientBuilder.default[IO].build.use { httpClient =>
              val heatersClient = HeatersClient(httpClient, settings.heaters)
              val service = new HeatersService(heatersClient, settings.heaters)

              action match {
                case HeatersCmd.StatusAction =>
                  service.getStatus().map { status =>
                    val json = HeatersHttpService.statusEncoder(status)
                    println(json.spaces4)
                    ExitCode.Success
                  }
                case HeatersCmd.UpdateAction(channel, mode) =>
                  service.update(channel, mode).map { status =>
                    val json = HeatersHttpService.statusEncoder(status)
                    println(json.spaces4)
                    ExitCode.Success
                  }
              }
            }

          case FinanceCmd(action) =>
            action match {
              case FinanceCmd.ResetVolumeAction =>
                println("Reset volume")
                IO.pure(ExitCode.Success)

              case FinanceCmd.SetupVolumeAction =>
                println("Setup volume")
                IO.pure(ExitCode.Success)
            }
        }

      case Left(error) =>
        ???
    }
  }
}

case class FinanceCmd(action: FinanceCmd.Action) extends SreCmd

object FinanceCmd {

  sealed trait Action
  case object ResetVolumeAction extends Action
  case object SetupVolumeAction extends Action

  val cmd = Command(
    name = "finance",
    header = "Finance operations"
  ) {
    val resetOpt = Opts.flag("reset", help = "Reset volume").map(_ => ResetVolumeAction)
    val setupVolumeOpt = Opts.flag("setup", help = "Setup volume").map { _ => SetupVolumeAction }

    resetOpt orElse setupVolumeOpt map FinanceCmd.apply
  }
}

case class HeatersCmd(action: HeatersCmd.Action) extends SreCmd

object HeatersCmd {

  sealed trait Action
  case object StatusAction extends Action
  case class UpdateAction(channel: Int, mode: Mode) extends Action

  val statusCmd = Command(
    name = "status",
    header = "Get heaters status"
  ) {
    Opts(StatusAction)
  }

  val updateCmd = Command(
    name = "update",
    header = "Set channel mode"
  ) {
    val channelOpt = Opts.option[Int]("channel", help = "Channel").mapValidated { i =>
      if (i >= 0 && i <= 3) {
        Validated.valid(i)
      } else {
        Validated.invalidNel(s"Invalid channel value: $i")
      }
    }

    val modeOpt = Opts.option[Int]("mode", help = "Channel").mapValidated { i =>
      Mode.get(i) match {
        case Some(mode) =>
          Validated.valid(mode)
        case None =>
          Validated.invalidNel(s"Invalid mode value: $i")
      }
    }

    (channelOpt, modeOpt).mapN {
      case (channel, mode) =>
        UpdateAction(channel, mode)
    }
  }

  val cmd = Command(
    name = "heaters",
    header = "Heater operations"
  ) {
    val statusOpt: Opts[Action] = Opts.subcommand(statusCmd)
    val updateOpt: Opts[Action] = Opts.subcommand(updateCmd)

    statusOpt orElse updateOpt map (HeatersCmd(_))
  }
}
