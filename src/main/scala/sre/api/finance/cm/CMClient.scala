package sre.api.finance
package cm

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import fs2.Stream
import cats.Parallel
import cats.data.EitherT
import cats.implicits._
import cats.effect._
import cats.effect.concurrent.{ Ref, Deferred }
import org.http4s._
import org.http4s.client._
import fs2.concurrent.SignallingRef
import org.slf4j.{ LoggerFactory, Logger }
import sre.api.{ Settings, CMSettings }
import ofx.OfxStmTrn

case class CMClient[F[_]](
  httpClient: Client[F],
  settings: CMSettings,
  basicAuthSessionRef: Ref[F, Option[Deferred[F, CMBasicAuthSession]]],
  otpSessionRef: Ref[F, Option[Deferred[F, CMOtpSession]]],
  otpPollingInterrupter: SignallingRef[F, Boolean],
  otpSessionFile: CMOtpSessionFile[F],
  formCache: CMDownloadFormCache,
  balancesCache: CMBalancesCache,
  ofxCache: CMOfxExportCache,
  csvCache: CMCsvExportCache,
  logger: Logger
)(implicit F: ConcurrentEffect[F], timer: Timer[F], parallel: Parallel[F]) extends CMClientDsl[F] {

  private val FORMATTER = DateTimeFormatter.ofPattern("dd/MM/yyyy")

  def fetchDownloadForm(): EitherT[F, CMOtpRequest, CMDownloadForm] = {
    formCache.cached {
      doAuthenticatedGET(settings.downloadUri) { response =>
        response.as[String].map(CMDownloadForm.parseOrFail)
      }
    }
  }

  private def fetchAccountStateByInput(input: CMAccountInput): EitherT[F, CMOtpRequest, CMAccountState] = {
    fetchStatements(input.id).map { statements =>
      settings.accounts.find(_.id == input.id) match {
        case Some(accountSettings) =>
          CMAccountState(
            id = input.id,
            `type` = accountSettings.`type`,
            label = input.label,
            displayName = Some(accountSettings.label),
            statements = statements
          )

        case None =>
          CMAccountState.unknown(
            id = input.id,
            label = input.label,
            statements = statements
          )
      }
    }
  }

  def fetchAccountState(accountId: String): EitherT[F, CMOtpRequest, Option[CMAccountState]] = {
    fetchDownloadForm().flatMap { downloadForm =>
      downloadForm.inputs.find(_.id == accountId) match {
        case Some(input) => fetchAccountStateByInput(input).map(account => Some(account))
        case None => EitherT.right(F.pure(None))
      }
    }
  }

  def fetchAccountsState(): EitherT[F, CMOtpRequest, List[CMAccountState]] = {
    fetchDownloadForm().flatMap { downloadForm =>
      downloadForm.inputs.grouped(4).toList.map { group =>
        EitherT(group.map(fetchAccountStateByInput(_).value).parSequence.map(_.sequence))
      }.sequence.map(_.flatten)
    }
  }


  def fetchStatements(accountId: String, maybeStartDate: Option[LocalDate] = None, maybeEndDate: Option[LocalDate] = None): EitherT[F, CMOtpRequest, List[CMStatement]] = {
    exportAsCSV(accountId, maybeStartDate, maybeEndDate).map { csvRecords =>
      csvRecords.zipWithIndex.map {
        case (csvRecord, index) =>
          val id = s"${accountId}${index}"
          csvRecord.toStatement(id, accountId)
      }
    }
  }

  def exportAsOfx(accountId: String, maybeStartDate: Option[LocalDate] = None, maybeEndDate: Option[LocalDate] = None, retries: Int = 1): EitherT[F, CMOtpRequest, List[OfxStmTrn]] =
    ofxCache.cached(accountId, maybeStartDate, maybeEndDate) {
      fetchDownloadForm().flatMap { downloadForm =>
        val action = settings.baseUri.withPath(downloadForm.action)
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
          "[t:dbt%3adate;]data_daterange_enddate_value"-> endDate,
          input.checkName -> "on",
          "_FID_DoDownload.x" -> "0",
          "_FID_DoDownload.y" -> "0"
        )

        doAuthenticatedPOST(action, data)(_.as[String]).flatMap { body =>
          CMDownloadForm.parse(body) match {
            case Left(_) =>
              EitherT.right(OfxStmTrn.load(body))

            case Right(form) =>
              EitherT.right(formCache.set(form)).flatMap { _ =>
                if (retries > 0) {
                  exportAsOfx(accountId, maybeStartDate, maybeEndDate, retries - 1)
                } else {
                  sys.error("Unable to export ofx")
                }
              }
          }
        }
      }
    }

  def exportAsCSV(accountId: String, maybeStartDate: Option[LocalDate] = None, maybeEndDate: Option[LocalDate] = None, retries: Int = 1): EitherT[F, CMOtpRequest, List[CMCsvRecord]] =
    csvCache.cached(accountId, maybeStartDate, maybeEndDate) {
      fetchDownloadForm().flatMap { downloadForm =>
        val uri = settings.baseUri.withPath(downloadForm.action)

        val input = downloadForm.inputs.find(_.id == accountId) getOrElse {
          sys.error(s"Unknown account $accountId")
        }

        val startDate = maybeStartDate map(_.format(FORMATTER)) getOrElse ""
        val endDate = maybeEndDate map(_.format(FORMATTER)) getOrElse ""

        val data = UrlForm(
          "data_formats_selected" -> "csv",
          "data_formats_options_csv_fileformat" -> "2",
          "data_formats_options_csv_dateformat" -> "0",
          "data_formats_options_csv_fieldseparator" -> "0",
          "data_formats_options_csv_amountcolnumber" -> "0",
          "data_formats_options_csv_decimalseparator" -> "1",
          "data_daterange_value" -> "1",
          "[t:dbt%3adate;]data_daterange_startdate_value" -> startDate,
          "[t:dbt%3adate;]data_daterange_enddate_value"-> endDate,
          input.checkName -> "on",
          "_FID_DoDownload.x" -> "0",
          "_FID_DoDownload.y" -> "0"
        )

        doAuthenticatedPOST(uri, data)(_.as[String]).flatMap { body =>
          CMDownloadForm.parse(body) match {
            case Left(_) =>
              val lines = body.split("\n").toList
              EitherT.right(F.pure(lines.tail.map(CMCsvRecord.parseOrFail)))

            case Right(form) =>
              EitherT.liftT(formCache.set(form)).flatMap { _ =>
                if (retries > 0) {
                  exportAsCSV(accountId, maybeStartDate, maybeEndDate, retries - 1)
                } else {
                  sys.error("Unable to export csv")
                }
              }
          }
        }
      }
    }
}

object CMClient {

  def stream[F[_]: ConcurrentEffect : Timer : Parallel](httpClient: Client[F], settings: Settings): Stream[F, CMClient[F]] = {
    val logger = LoggerFactory.getLogger("sre.api.finance.CmClient")

    val otpSessionFile = settings.finance.cm.otpSessionFile

    val client = for {
      basicAuthSessionRef <- Ref.of[F, Option[Deferred[F, CMBasicAuthSession]]](None)

      otpPollingInterrupter <- SignallingRef[F, Boolean](false)

      maybeValidOptSession <- otpSessionFile.get.map {
        case Left(error) =>
          logger.warn(s"Unable to restore otp session from ${settings.finance.cm.otpSession}:\n$error")
          None

        case Right(otpSession) =>
          Some(otpSession)
      }.value.map(_.flatten)

      maybeDeferredOptSessionRef <- maybeValidOptSession.map { otpSession =>
        logger.info(s"Restoring opt session with $otpSession")
        Deferred[F, CMOtpSession].flatMap { d => d.complete(otpSession).map(_ => d) }
      }.sequence

      otpSessionRef <- Ref.of[F, Option[Deferred[F, CMOtpSession]]](maybeDeferredOptSessionRef)
    } yield {
      val formCache = CMDownloadFormCache(settings.finance.cm.cache.form)
      val balancesCache = CMBalancesCache(settings.finance.cm.cache.balances)
      val ofxCache = CMOfxExportCache(settings.finance.cm.cache.ofx)
      val csvCache = CMCsvExportCache(settings.finance.cm.cache.csv)

      CMClient[F](
        httpClient,
        settings.finance.cm,
        basicAuthSessionRef,
        otpSessionRef,
        otpPollingInterrupter,
        otpSessionFile,
        formCache,
        balancesCache,
        ofxCache,
        csvCache,
        logger
      )
    }
    Stream.eval(client)
  }
}
