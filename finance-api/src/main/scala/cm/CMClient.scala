package sre.api.finance
package cm

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import fs2.Stream
import cats.Parallel
import cats.data.EitherT
import cats.implicits._
import cats.effect._
import org.http4s._
import org.http4s.client._
import fs2.concurrent.SignallingRef
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import sre.api.settings._

case class CMClient[F[_]: Parallel: Logger](
    logger: Logger[F],
    httpClient: Client[F],
    settings: CMSettings,
    basicAuthSessionRef: Ref[F, Option[Deferred[F, CMBasicAuthSession]]],
    otpSessionRef: Ref[F, Option[Deferred[F, CMOtpSession]]],
    otpSessionFile: CMOtpSessionFile[F]
)(implicit F: Concurrent[F])
    extends CMClientDsl[F] {

  private val FORMATTER = DateTimeFormatter.ofPattern("dd/MM/yyyy")

  def fetchDownloadForm(): EitherT[F, CMOtpRequest, CMDownloadForm] = {
    authenticatedGet(settings.downloadUri) { response =>
      response.as[String].map(CMDownloadForm.parseOrFail)
    }
  }

  private def fetchAccountStateByInput(
      input: CMAccountInput
  ): EitherT[F, CMOtpRequest, CMAccountState] = {
    fetchStatements(input.id).map { statements =>
      settings.accounts.find(_.id == input.id) match {
        case Some(accountSettings) =>
          CMAccountState(
            id = input.id,
            `type` = accountSettings.`type`,
            label = Some(input.label),
            displayName = Some(accountSettings.label),
            statements = statements
          )

        case None =>
          CMAccountState.unknown(
            id = input.id,
            label = Some(input.label),
            statements = statements
          )
      }
    }
  }

  def fetchAccountsState(): EitherT[F, CMOtpRequest, List[CMAccountState]] = {
    for {
      _ <- EitherT.liftT(logger.info(s"Fetching accounts state"))
      accountsState <- fetchDownloadForm().flatMap { downloadForm =>
        downloadForm.inputs
          .grouped(4)
          .toList
          .map { group =>
            EitherT(
              group
                .map(fetchAccountStateByInput(_).value)
                .parSequence
                .map(_.sequence)
            )
          }
          .sequence
          .map(_.flatten)
      }
    } yield accountsState
  }

  def fetchStatements(
      accountId: String,
      maybeStartDate: Option[LocalDate] = None,
      maybeEndDate: Option[LocalDate] = None
  ): EitherT[F, CMOtpRequest, List[CMStatement]] = {
    fetchCSVRecords(accountId, maybeStartDate, maybeEndDate).map { csvRecords =>
      csvRecords.zipWithIndex.map { case (csvRecord, index) =>
        val id = s"${accountId}${index}"
        csvRecord.toStatement(id, accountId)
      }
    }
  }

  def fetchAccountOfxStmTrn[A](
      accountId: String,
      maybeStartDate: Option[LocalDate] = None,
      maybeEndDate: Option[LocalDate] = None
  )(f: Response[F] => F[A]): EitherT[F, CMOtpRequest, A] =
    fetchDownloadForm().flatMap { downloadForm =>
      val uri = settings.baseUri.withPath(downloadForm.action)
      val input = downloadForm.inputs.find(_.id == accountId) getOrElse {
        sys.error(s"Unknown account $accountId")
      }

      val startDate = maybeStartDate.map(_.format(FORMATTER)) getOrElse ""
      val endDate = maybeEndDate.map(_.format(FORMATTER)) getOrElse ""

      val data = UrlForm(
        "data_formats_selected" -> "ofx",
        "data_formats_options_ofx_fileformat" -> "ofx-format-m2003",
        "data_daterange_value" -> "1",
        "[t:dbt%3adate;]data_daterange_startdate_value" -> startDate,
        "[t:dbt%3adate;]data_daterange_enddate_value" -> endDate,
        input.checkName -> "on",
        "_FID_DoDownload.x" -> "0",
        "_FID_DoDownload.y" -> "0"
      )

      authenticatedPost(uri, data)(f)
    }

  def fetchAccountsOfxStmTrn[A](
      maybeStartDate: Option[LocalDate] = None,
      maybeEndDate: Option[LocalDate] = None
  )(f: (String, Response[F]) => F[A]): EitherT[F, CMOtpRequest, List[A]] =
    fetchDownloadForm().flatMap { downloadForm =>
      downloadForm.inputs
        .grouped(4)
        .toList
        .map { group =>
          EitherT(
            group
              .map { input =>
                fetchAccountOfxStmTrn(input.id)(response =>
                  f(input.id, response)
                ).value
              }
              .toList
              .parSequence
              .map(_.sequence)
          )
        }
        .sequence
        .map(_.flatten)
    }

  def fetchCSVRecords(
      accountId: String,
      maybeStartDate: Option[LocalDate] = None,
      maybeEndDate: Option[LocalDate] = None,
      retries: Int = 1
  ): EitherT[F, CMOtpRequest, List[CMCsvRecord]] =
    fetchDownloadForm().flatMap { downloadForm =>
      val uri = settings.baseUri.withPath(downloadForm.action)

      val input = downloadForm.inputs.find(_.id == accountId) getOrElse {
        sys.error(s"Unknown account $accountId")
      }

      val startDate = maybeStartDate map (_.format(FORMATTER)) getOrElse ""
      val endDate = maybeEndDate map (_.format(FORMATTER)) getOrElse ""

      val data = UrlForm(
        "data_formats_selected" -> "csv",
        "data_formats_options_csv_fileformat" -> "2",
        "data_formats_options_csv_dateformat" -> "0",
        "data_formats_options_csv_fieldseparator" -> "0",
        "data_formats_options_csv_amountcolnumber" -> "0",
        "data_formats_options_csv_decimalseparator" -> "1",
        "data_daterange_value" -> "1",
        "[t:dbt%3adate;]data_daterange_startdate_value" -> startDate,
        "[t:dbt%3adate;]data_daterange_enddate_value" -> endDate,
        input.checkName -> "on",
        "_FID_DoDownload.x" -> "0",
        "_FID_DoDownload.y" -> "0"
      )

      authenticatedPost(uri, data)(_.as[String]).flatMap { body =>
        CMDownloadForm.parse(body) match {
          case Left(_) =>
            val lines = body.split("\n").toList
            EitherT.right(F.pure(lines.tail.map(CMCsvRecord.parseOrFail)))

          case Right(form) =>
            if (retries > 0) {
              fetchCSVRecords(
                accountId,
                maybeStartDate,
                maybeEndDate,
                retries - 1
              )
            } else {
              sys.error("Unable to export csv")
            }
        }
      }
    }
}

object CMClient {

  def resource[F[_]: Async: Concurrent: Parallel](
      httpClient: Client[F],
      settings: FinanceSettings
  ): Resource[F, CMClient[F]] = {
    implicit def logger[F[_]: Sync]: Logger[F] = Slf4jLogger.getLogger[F]

    val otpSessionFile = CMOtpSessionFile(settings.cm.otpSession)

    val client = for {
      basicAuthSessionRef <- Ref[F].of[Option[Deferred[F, CMBasicAuthSession]]](
        None
      )

      maybeValidOptSession <- otpSessionFile.get
        .map {
          case Left(error) =>
            logger.warn(
              s"Unable to restore otp session from ${settings.cm.otpSession}:\n$error"
            )
            None

          case Right(otpSession) =>
            Some(otpSession)
        }
        .value
        .map(_.flatten)

      maybeDeferredOptSessionRef <- maybeValidOptSession.map { otpSession =>
        logger.info(s"Restoring opt session with $otpSession")
        Deferred[F, CMOtpSession].flatMap { d =>
          d.complete(otpSession).map(_ => d)
        }
      }.sequence

      otpSessionRef <- Ref.of(maybeDeferredOptSessionRef)
    } yield {
      CMClient[F](
        logger,
        httpClient,
        settings.cm,
        basicAuthSessionRef,
        otpSessionRef,
        otpSessionFile
      )
    }
    Resource.eval(client)
  }
}
