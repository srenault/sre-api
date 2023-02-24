package sre.api

import java.util.concurrent.CompletableFuture
import java.nio.file.Path
import java.io.{InputStream, FileOutputStream}
import scala.compat.java8.FutureConverters._
import software.amazon.awssdk.auth.credentials._
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.s3.{S3Client => AwsS3Client, _}
import software.amazon.awssdk.services.s3.model._
import software.amazon.awssdk.http.nio.netty.NettyNioAsyncHttpClient
import scala.jdk.CollectionConverters._
import cats.effect._
import cats.implicits._
import fs2._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import settings.S3Settings

case class S3ObjectListing(
    objects: List[S3Object],
    continuationToken: Option[String]
)

case class S3Client[F[_]: Logger](bucket: String, s3Client: S3AsyncClient)(
    implicit F: Async[F]
) {

  def completableFutureToIO[T](cf: CompletableFuture[T]): F[T] =
    F.fromFuture(F.delay(toScala(cf)))

  def ls(
      prefix: String,
      maxKeys: Int,
      continuationToken: Option[String]
  ): F[S3ObjectListing] = {
    for {
      _ <- Logger[F].info(s"Listing $prefix...")
      listing <- {
        val request = {
          val reqBuilder = ListObjectsV2Request
            .builder()
            .bucket(bucket)
            .prefix(prefix)

          continuationToken
            .fold(reqBuilder)(reqBuilder.continuationToken(_))
            .maxKeys(maxKeys)
            .build()
        }

        completableFutureToIO(s3Client.listObjectsV2(request)).map { response =>
          val objects = response.contents().asScala.toList
          val nextContinuationToken = Option(response.nextContinuationToken())
          S3ObjectListing(objects, nextContinuationToken)
        }
      }
      _ <- Logger[F].info(s"Found ${listing.objects.length} S3 objects")
    } yield listing
  }

  def downloadFileTo(key: String, destinationPath: Path): F[Unit] = {
    val req: GetObjectRequest = GetObjectRequest
      .builder()
      .key(key)
      .bucket(bucket)
      .build()

    val f = s3Client.getObject(req, destinationPath)

    for {
      _ <- Logger[F].info(s"Downloading $key to $destinationPath ...")

      _ <- completableFutureToIO(f)

      _ <- Logger[F].info(s"$key successfuly downloaded to $destinationPath")

    } yield ()
  }
}

object S3Client {

  def apply[F[_]: Async](settings: S3Settings): S3Client[F] = {
    implicit def logger[F[_]: Sync]: Logger[F] = Slf4jLogger.getLogger[F]

    val credentials =
      AwsBasicCredentials.create(settings.publicKey, settings.secretKey)

    val credentialsProvider = StaticCredentialsProvider.create(credentials)

    val httpClientBuilder = NettyNioAsyncHttpClient.builder()

    val s3Client = S3AsyncClient
      .builder()
      .region(Region.of(settings.region))
      .httpClientBuilder(httpClientBuilder)
      .credentialsProvider(credentialsProvider)
      .build()

    S3Client(settings.bucket, s3Client)
  }
}
