package sre.api
package finance

import java.nio.file.Path
import java.io.File
import java.time.YearMonth
import cats.Parallel
import cats.data.EitherT
import cats.effect._
import cats.implicits._
import org.http4s._
import org.http4s.circe._
import io.circe.literal._
import io.circe.syntax._
import org.typelevel.log4cats.Logger
import analytics.{ Period, PeriodIndex }
import cm._
import analytics.AnalyticsClient
import settings.FinanceSettings

case class FinanceTasks[F[_] : Logger : Parallel](
  cmClient: CMClient[F],
  dbClient: DBClient[F],
  settings: FinanceSettings
)(implicit F: Async[F]) {

  lazy val s3Client = S3Client[F](settings.s3)

  lazy val analyticsClient = AnalyticsClient(dbClient, settings)

  def resetVolume(): F[Unit] = {
    def deleteRecursively(file: File): F[Unit] =
      Logger[F].info(s"Deleting ${file.getAbsolutePath}").flatMap { _ =>
        if (file.isDirectory) {
          file.listFiles.toList.map(f => deleteRecursively(f)).parSequence.map { _ =>
            if (file.exists && !file.delete) {
              sys.error(s"Unable to delete ${file.getAbsolutePath}")
            }
          }
        } else {
          if (file.exists && !file.delete) {
            sys.error(s"Unable to delete ${file.getAbsolutePath}")
          } else {
            F.pure(())
          }
        }
      }

    deleteRecursively(settings.transactionsDir.toFile)
  }

  def setupVolume(continuationToken: Option[String] = None): F[Option[String]] = {
    for {
      _ <- Logger[F].info(s"Listing transactions to download [continuationToken=${continuationToken}]")

      maxKeys = settings.setupVolume.maxKeys

      listing <- s3Client.ls(settings.s3.prefix, maxKeys, continuationToken)

      _ <- Logger[F].info(s"Downloading transactions...")

      _ <- listing.objects.map { obj =>
        val s = obj.key.replace(settings.s3.prefix, "")
        val destinationPath = settings.transactionsDir.resolve(s)
        val parentDir = destinationPath.toFile.getParent
        new File(parentDir).mkdirs
        s3Client.downloadFileTo(obj.key, destinationPath)
      }.parSequence

      _ <- Logger[F].info(s"Transactions downloaded [nextContinuationToken=${listing.continuationToken}]")

    } yield listing.continuationToken
  }

  def snapshot(): EitherT[F, CMOtpRequest, List[Path]] = {
    for {
      _ <- EitherT.liftF(Logger[F].info(s"Refresh current period"))

      files <- cmClient.fetchAccountsOfxStmTrn() {
        case (accountId, response) =>
          val accountPath = settings.transactionsDir.resolve(accountId)
          finance.ofx.OfxStmTrn.persist(is = response.body, accountPath).map(_ => accountPath)
      }

    } yield files
  }

  def reindex(fromScratch: Boolean): F[List[PeriodIndex]] = {
    for {
      _ <- Logger[F].info(s"Reindexing [fromScratch=${fromScratch}]")

      periods <- analyticsClient.reindex(fromScratch)

    } yield periods
  }

  def importStatements(): EitherT[F, CMOtpRequest, List[CMAccountState]] = {
    for {
      _ <- EitherT.liftF(Logger[F].info(s"Import statements"))

      accountsState <- cmClient.fetchAccountsState()

    } yield accountsState
  }

  def checkOtpStatus(transactionId: String): F[CMOtpStatus] = {
    cmClient.checkOtpStatus(transactionId)
  }
}
