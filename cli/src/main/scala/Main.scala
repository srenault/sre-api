package sre.cli

import java.time.YearMonth
import java.time.format.DateTimeFormatterBuilder
import scala.concurrent.ExecutionContext.Implicits.global
import io.circe._
import io.circe.literal._
import io.circe.syntax._
import cats.effect._
import cats.implicits._
import cats.data.Validated
import com.monovore.decline._
import com.monovore.decline.effect._
import java.nio.file.Path
import org.http4s.client.blaze._
import org.http4s.client._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import sre.api.settings._
import sre.api.heaters._

sealed trait SreCmd

object Cli
    extends CommandIOApp(
      name = "sre",
      header = "sreapi command line",
      version = "0.0.1"
    ) {

  implicit def logger[F[_]: Sync]: Logger[F] = Slf4jLogger.getLogger[F]

  def heatersHandler(
      action: HeatersCmd.Action,
      settings: Settings
  ): IO[ExitCode] = {
    BlazeClientBuilder[IO](global).resource.use { httpClient =>
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
  }

  def financeHandler(
      action: FinanceCmd.Action,
      settings: Settings
  ): IO[ExitCode] = {
    import sre.api.finance._
    import sre.api.finance.cm._

    (for {
      httpClient <- BlazeClientBuilder[IO](global).resource
      dbClient <- DBClient.resource[IO](settings.finance)
      cmClient <- CMClient.resource(httpClient, settings.finance)
    } yield (httpClient, dbClient, cmClient)).use {
      case (httpClient, dbClient, cmClient) =>
        val service = new FinanceService(cmClient, dbClient, settings.finance)
        val tasks = new FinanceTasks(cmClient, dbClient, settings.finance)

        action match {
          case FinanceCmd.ResetVolumeAction =>
            tasks.resetVolume().map(_ => ExitCode.Success)

          case FinanceCmd.SetupVolumeAction(maybeContinuationToken) =>
            tasks.setupVolume(maybeContinuationToken).map {
              maybeNextContinuationToken =>
                val token = maybeNextContinuationToken.getOrElse("N/A")
                println(json"""{ "nextContinuationToken": $token }""")
                ExitCode.Success
            }

          case FinanceCmd.ReindexAction(fromScratch) =>
            tasks.reindex(fromScratch).map { periods =>
              println(periods.asJson.spaces4)
              ExitCode.Success
            }

          case FinanceCmd.SnapshotAction =>
            tasks.snapshot().value.map {
              case Right(files) =>
                println(files.map(_.toString).asJson.spaces4)
                ExitCode.Success

              case Left(otpRequest) =>
                println(json"""{ "otpRequest": $otpRequest }""")
                ExitCode.Success
            }

          case FinanceCmd.CheckOtpAction(otpRequest) =>
            tasks.checkOtpStatus(otpRequest).map { status =>
              println(status.asJson.spaces4)
              ExitCode.Success
            }

          case FinanceCmd.GetAccountsAction =>
            service.getAccountsOverview().map { accountsOverview =>
              println(accountsOverview.asJson.spaces4)
              ExitCode.Success
            }

          case FinanceCmd.GetAccountAction(accountId, maybePeriodDate) =>
            service.getAccountState(accountId, maybePeriodDate).map {
              accountState =>
                println(accountState.asJson.spaces4)
                ExitCode.Success
            }

          case FinanceCmd.GetPeriodsAction(
                maybeBeforePeriod,
                maybeAfterPeriod
              ) =>
            service.getPeriods(maybeBeforePeriod, maybeAfterPeriod).map {
              periods =>
                println(periods.asJson.spaces4)
                ExitCode.Success
            }

          case FinanceCmd.GetStatementsForPeriodAction(period) =>
            service.getStatementsForPeriod(period).map { statements =>
              println(statements.asJson.spaces4)
              ExitCode.Success
            }
        }
    }
  }

  def main: Opts[IO[ExitCode]] = {
    val configOpt =
      Opts.option[Path]("config", help = "Local path to configuration file")

    //    configOpt.orNone.map { maybeConfigPath =>
    Settings.load(maybeConfigPath = None) match {
      case Right(settings) =>
        val financeOpt: Opts[SreCmd] = Opts.subcommand(FinanceCmd.cmd)
        val heaterOpt: Opts[SreCmd] = Opts.subcommand(HeatersCmd.cmd)

        (financeOpt orElse heaterOpt).map {
          case HeatersCmd(action) =>
            heatersHandler(action, settings)

          case FinanceCmd(action) =>
            financeHandler(action, settings)
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
  case object SnapshotAction extends Action
  case class CheckOtpAction(transactionId: String) extends Action
  case class ReindexAction(fromScratch: Boolean) extends Action
  case class SetupVolumeAction(continuationToken: Option[String]) extends Action
  case class GetAccountAction(
      accountId: String,
      maybePeriodDate: Option[YearMonth]
  ) extends Action
  case object GetAccountsAction extends Action
  case class GetPeriodsAction(
      maybeBeforePeriod: Option[YearMonth],
      maybeAfterPeriod: Option[YearMonth]
  ) extends Action
  case class GetStatementsForPeriodAction(period: YearMonth) extends Action

  implicit val yearMonthArgument: Argument[YearMonth] =
    new Argument[YearMonth] {
      def read(str: String) = {
        Validated
          .catchNonFatal {
            val format = new DateTimeFormatterBuilder()
              .appendPattern("yyyy-MM")
              .toFormatter();

            YearMonth.parse(str, format)
          }
          .leftMap(t => s"Invalid period date: ${t.getMessage}")
          .toValidatedNel
      }

      def defaultMetavar = "key:value"
    }

  val resetCmd = Command(
    name = "reset",
    header = "Reset volume"
  ) {
    Opts(ResetVolumeAction)
  }

  val setupCmd = Command(
    name = "setup",
    header = "Setup volume"
  ) {
    Opts
      .option[String]("continuationToken", help = "Continuation token")
      .orNone
      .map { maybeContinuationToken =>
        SetupVolumeAction(maybeContinuationToken)
      }
  }

  val snapshotCmd = Command(
    name = "snapshot",
    header = "Snapshot"
  ) {
    Opts(SnapshotAction)
  }

  val reindexCmd = Command(
    name = "reindex",
    header = "Reindex"
  ) {
    Opts.flag("fromScratch", help = "Reindex from scratch").orFalse.map {
      fromScratch =>
        ReindexAction(fromScratch)
    }
  }

  val accountsCmd = Command(
    name = "accounts",
    header = "Accounts overview"
  ) {
    Opts(GetAccountsAction)
  }

  val accountCmd = Command(
    name = "account",
    header = "Account info"
  ) {

    val accountIdOpt = Opts.option[String]("accountId", help = "Account id")

    val periodDateOpt =
      Opts.option[YearMonth]("periodDate", help = "Period date").orNone

    (accountIdOpt, periodDateOpt).mapN { case (accountId, maybePeriodDate) =>
      GetAccountAction(accountId, maybePeriodDate)
    }
  }

  val periodsCmd = Command(
    name = "periods",
    header = "Display periods"
  ) {
    val beforePeriodOpt =
      Opts.option[YearMonth]("before", help = "Get periods before").orNone
    val afterPeriodOpt =
      Opts.option[YearMonth]("after", help = "Get periods after").orNone

    (beforePeriodOpt, afterPeriodOpt).mapN {
      case (maybeBeforePeriod, maybeAfterPeriod) =>
        GetPeriodsAction(maybeBeforePeriod, maybeAfterPeriod)
    }
  }

  val statementsForPeriodCmd = Command(
    name = "statements",
    header = "Display statements accross accounts for period"
  ) {
    Opts.option[YearMonth]("period", help = "period").map { period =>
      GetStatementsForPeriodAction(period)
    }
  }

  val cmd = Command(
    name = "finance",
    header = "Finance operations"
  ) {
    val resetOpt = Opts.subcommand(resetCmd)
    val setupOpt = Opts.subcommand(setupCmd)
    val reindexOpt = Opts.subcommand(reindexCmd)
    val snapshotOpt = Opts.subcommand(snapshotCmd)
    val accountsOpt = Opts.subcommand(accountsCmd)
    val accountOpt = Opts.subcommand(accountCmd)
    val periodsOpt = Opts.subcommand(periodsCmd)
    val statementsForPeriodOpt = Opts.subcommand(statementsForPeriodCmd)

    resetOpt orElse
      setupOpt orElse
      reindexOpt orElse
      accountsOpt orElse
      accountOpt orElse
      periodsOpt orElse
      snapshotOpt orElse
      statementsForPeriodOpt map (FinanceCmd(_))
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
    val channelOpt =
      Opts.option[Int]("channel", help = "Channel").mapValidated { i =>
        if (i >= 0 && i <= 3) {
          Validated.valid(i)
        } else {
          Validated.invalidNel(s"Invalid channel value: $i")
        }
      }

    val modeOpt = Opts.option[Int]("mode", help = "Mode").mapValidated { i =>
      Mode.get(i) match {
        case Some(mode) =>
          Validated.valid(mode)
        case None =>
          Validated.invalidNel(s"Invalid mode value: $i")
      }
    }

    (channelOpt, modeOpt).mapN { case (channel, mode) =>
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
