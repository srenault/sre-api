package sre.dashboard.finance

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import fs2.Stream
import cats.implicits._
import cats.effect._
import cats.effect.concurrent.{ Ref, Deferred }
import org.http4s._
import org.http4s.client._
import sre.dashboard.{ Settings, CMSettings }
import org.slf4j.{ LoggerFactory, Logger }

case class CMClient[F[_]](
  httpClient: Client[F],
  settings: CMSettings,
  sessionRef: Ref[F, Option[Deferred[F, CMSession]]],
  formCache: CMDownloadFormCache,
  balancesCache: CMBalancesCache,
  ofxCache: CMOfxExportCache,
  csvCache: CMCsvExportCache,
  logger: Logger
)(implicit F: ConcurrentEffect[F]) extends CMClientDsl[F] {

  private val FORMATTER = DateTimeFormatter.ofPattern("dd/MM/yyyy")

  def fetchDownloadForm(): F[CMDownloadForm] =
    formCache.cached {
      doAuthenticatedGET(settings.downloadUri) { response =>
        response.as[String].map(CMDownloadForm.parseOrFail)
      }
    }

  def fetchBalance(accountId: String): F[Float] = {
    balancesCache.cached(accountId) {
      exportAsCSV(accountId).map { lines =>
        lines.lastOption.map(_.balance).getOrElse {
          sys.error(s"Unable to get balance for $accountId")
        }
      }
    }
  }

  def fetchAccounts(): F[List[CMAccount]] = {
    fetchDownloadForm().flatMap { downloadForm =>
      downloadForm.inputs.map { input =>
        fetchBalance(input.id).map { balance =>
          CMAccount(input, balance)
        }
      }.sequence
    }
  }

  def exportAsOfx(accountId: String, maybeStartDate: Option[LocalDate] = None, maybeEndDate: Option[LocalDate] = None, retries: Int = 1): F[List[OfxStmTrn]] =
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

        doAuthenticatedPOST(action, data) { response =>
          response.as[String].flatMap { body =>
            CMDownloadForm.parse(body) match {
              case Left(_) =>
                OfxStmTrn.load(body)
              case Right(form) =>
                formCache.set(form)
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

  def exportAsCSV(accountId: String, maybeStartDate: Option[LocalDate] = None, maybeEndDate: Option[LocalDate] = None, retries: Int = 1): F[List[CMCsvRecord]] =
    csvCache.cached(accountId, maybeStartDate, maybeEndDate) {
      fetchDownloadForm().flatMap { downloadForm =>
        val action = settings.baseUri.withPath(downloadForm.action)

        val input = downloadForm.inputs.find(_.id == accountId) getOrElse {
          sys.error(s"Unknown account $accountId")
        }

        val startDate = maybeStartDate.map(_.format(FORMATTER)) getOrElse ""
        val endDate = maybeEndDate.map(_.format(FORMATTER)) getOrElse ""

        val data = UrlForm(
          "data_formats_selected" -> "csv",
          "data_formats_options_csv_fileformat" -> "2",
          "data_formats_options_csv_dateformat" -> "0",
          "data_formats_options_csv_fieldseparator" -> "0",
          "data_formats_options_csv_amountcolnumber" -> "0",
          "data_formats_options_csv_decimalseparator" -> "1",
          "[t:dbt%3adate;]data_daterange_startdate_value" -> startDate,
          "[t:dbt%3adate;]data_daterange_enddate_value"-> endDate,
          input.checkName -> "on",
          "_FID_DoDownload.x" -> "0",
          "_FID_DoDownload.y" -> "0"
        )

        doAuthenticatedPOST(action, data) { response =>
          response.as[String].flatMap { body =>
            CMDownloadForm.parse(body) match {
              case Left(_) =>
                val lines = body.split("\n").toList
                F.pure { lines.tail.map(CMcsvLine.parseOrFail) }

              case Right(form) =>
                formCache.set(form).flatMap { _ =>
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
}

object CMClient {

  def stream[F[_]: ConcurrentEffect](httpClient: Client[F], settings: Settings): Stream[F, CMClient[F]] = {
    val client = for {
      d <- Deferred[F, CMSession]
      sessionRef <- Ref.of[F, Option[Deferred[F, CMSession]]](None)
    } yield {
      val formCache = CMDownloadFormCache(settings.finance.cm.cache.form)
      val balancesCache = CMBalancesCache(settings.finance.cm.cache.balances)
      val ofxCache = CMOfxExportCache(settings.finance.cm.cache.ofx)
      val csvCache = CMCsvExportCache(settings.finance.cm.cache.csv)
      val logger = LoggerFactory.getLogger("sre.dashboard.finance.CmClient")
      CMClient[F](httpClient, settings.finance.cm, sessionRef, formCache, balancesCache, ofxCache, csvCache, logger)
    }
    Stream.eval(client)
  }
}
