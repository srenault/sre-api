package sre.api

import java.nio.file.Path
import java.io.{ InputStream, FileOutputStream }
import software.amazon.awssdk.auth.credentials._
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.s3.{ S3Client => AwsS3Client }
import software.amazon.awssdk.services.s3.model._
import scala.jdk.CollectionConverters._
import cats.effect._
import cats.implicits._
import fs2._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

case class S3ObjectListing(objects: List[S3Object], continuationToken: Option[String])

case class S3Client[F[_] : Logger](bucket: String, prefix: Option[String], s3Client: AwsS3Client)(implicit F: Async[F]) {

  def ls(key: String, maxKeys: Int, continuationToken: Option[String]): F[S3ObjectListing] = {
    val dir = prefix.fold(key)(_ + "/" + key)

    for {
      _ <- Logger[F].info(s"Listing $dir...")
      listing <- F.pure {
        val request = {
          val reqBuilder = ListObjectsV2Request.builder()
            .bucket(bucket)
            .prefix(dir)

          continuationToken.fold(reqBuilder)(reqBuilder.continuationToken(_)).maxKeys(maxKeys).build()
        }

        val response = s3Client.listObjectsV2(request)
        val objects = response.contents().asScala.toList
        val nextContinuationToken = Option(response.nextContinuationToken())
        S3ObjectListing(objects, nextContinuationToken)
      }
      _ <- Logger[F].info(s"Found ${listing.objects.length} S3 objects")
    } yield listing
  }

  def downloadFileTo(objectPath: String, destinationPath: Path): F[Unit] = {
    val key = prefix.fold(objectPath)(_ + "/" + objectPath)
    val req = GetObjectRequest.builder()
      .key(key)
      .bucket(bucket)
      .build()

    val inputStream: InputStream = s3Client.getObjectAsBytes(req).asInputStream

    for {
      _ <- Logger[F].info(s"Downloading $key to $destinationPath ...")
      _ <- io.readInputStream(F.pure(inputStream), 4096)
      .through(io.file.writeAll(destinationPath))
      .compile
      .drain
      _ <- Logger[F].info(s"$key successfuly downloaded to $destinationPath")
    } yield ()
  }
}


object S3Client {

  def apply[F[_]: Async](settings: S3Settings): S3Client[F] = {
    implicit def logger[F[_]: Sync]: Logger[F] = Slf4jLogger.getLogger[F]

    val credentials = AwsBasicCredentials.create(settings.publicKey, settings.secretKey)

    val credentialsProvider = StaticCredentialsProvider.create(credentials)

    val s3Client = AwsS3Client.builder()
      .region(Region.of(settings.region))
      .credentialsProvider(credentialsProvider)
      .build()

    S3Client(settings.bucket, settings.prefix, s3Client)
  }
}
