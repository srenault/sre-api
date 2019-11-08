package sre.api.finance
package cm

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import cats.effect._
import cats.data.EitherT
import cats.implicits._
import scalacache._
import scalacache.guava._
import scalacache.CatsEffect.modes._
import com.google.common.cache.CacheBuilder
import sre.api.CMCacheSettings
import ofx.OfxStmTrn

case class CMBalancesCache(settings: CMCacheSettings) {

  private val cache: Cache[Float] = {
    val underlying = CacheBuilder.newBuilder().maximumSize(settings.size).build[String, Entry[Float]]
    GuavaCache(underlying)
  }

  def cached[F[_]: ConcurrentEffect](accountId: String)(f: => F[Float]): F[Float] = {
    cache.cachingF(accountId)(ttl = Some(settings.ttl))(f)
  }
}

case class CMDownloadFormCache(settings: CMCacheSettings) {

  private val KEY = "downloadForm"

  private val cache: Cache[CMDownloadForm] = {
    val underlying = CacheBuilder.newBuilder().maximumSize(settings.size).build[String, Entry[CMDownloadForm]]
    GuavaCache(underlying)
  }

  def cached[F[_]](f: EitherT[F, CMOtpRequest, CMDownloadForm])(implicit F: ConcurrentEffect[F]): EitherT[F, CMOtpRequest, CMDownloadForm] = {
    EitherT.liftT(cache.get(KEY)).flatMap {
      case Some(downloadForm) =>
        EitherT.right(F.pure(downloadForm))

      case None =>
        f.semiflatMap { downloadForm =>
          cache.caching(KEY)(ttl = Some(settings.ttl))(downloadForm)
        }
    }
  }

  def set[F[_]: ConcurrentEffect](form: CMDownloadForm): F[Unit] = {
    cache.put(KEY)(form, Some(settings.ttl)).map(_ => Unit)
  }
}

case class CMOfxExportCache(settings: CMCacheSettings) {

  private val dateFormat = DateTimeFormatter.ISO_LOCAL_DATE

  private val cache: Cache[List[OfxStmTrn]] = {
    val underlying = CacheBuilder.newBuilder().maximumSize(settings.size).build[String, Entry[List[OfxStmTrn]]]
    GuavaCache(underlying)
  }

  private def computeKey(accountId: String, startDate: Option[LocalDate], endDate: Option[LocalDate]): String =
    List(Some(accountId), startDate.map(_.format(dateFormat)), endDate.map(_.format(dateFormat))).flatten.mkString("#")

  def cached[F[_]](
    accountId: String,
    startDate: Option[LocalDate],
    endDate: Option[LocalDate]
  )(f: EitherT[F, CMOtpRequest, List[OfxStmTrn]])(implicit F: ConcurrentEffect[F]): EitherT[F, CMOtpRequest, List[OfxStmTrn]] = {
    val key = computeKey(accountId, startDate, endDate)
    EitherT.liftT(cache.get(key)).flatMap {
      case Some(statements) =>
        EitherT.right(F.pure(statements))

      case None =>
        f.semiflatMap { statements =>
          cache.caching(key)(ttl = Some(settings.ttl))(statements)
        }
    }
  }
}

case class CMCsvExportCache(settings: CMCacheSettings) {

  private val dateFormat = DateTimeFormatter.ISO_LOCAL_DATE

  private val cache: Cache[List[CMCsvRecord]] = {
    val underlying = CacheBuilder.newBuilder().maximumSize(settings.size).build[String, Entry[List[CMCsvRecord]]]
    GuavaCache(underlying)
  }

  private def computeKey(accountId: String, startDate: Option[LocalDate], endDate: Option[LocalDate]): String =
    List(Some(accountId), startDate.map(_.format(dateFormat)), endDate.map(_.format(dateFormat))).flatten.mkString("#")

  def cached[F[_]](
    accountId: String,
    startDate: Option[LocalDate],
    endDate: Option[LocalDate]
  )(f: EitherT[F, CMOtpRequest, List[CMCsvRecord]])(implicit F: ConcurrentEffect[F]): EitherT[F, CMOtpRequest, List[CMCsvRecord]] = {
    val key = computeKey(accountId, startDate, endDate)
    EitherT.liftT(cache.get(key)).flatMap {
      case Some(statements) =>
        EitherT.right(F.pure(statements))

      case None =>
        f.semiflatMap { statements =>
          cache.caching(key)(ttl = Some(settings.ttl))(statements)
        }
    }
  }
}
