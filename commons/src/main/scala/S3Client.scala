package sre.api

import java.nio.file.Path
import java.io.{ InputStream, FileOutputStream }
import com.amazonaws.auth.{ BasicAWSCredentials, AWSStaticCredentialsProvider }
import com.amazonaws.services.s3.model._
import com.amazonaws.services.s3.{ AmazonS3ClientBuilder, AmazonS3 }
import scala.jdk.CollectionConverters._
import cats.effect._
import cats.implicits._
import fs2._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

case class S3ObjectListing(objects: List[S3ObjectSummary], continuationToken: Option[String])

case class S3Client[F[_] : Logger](bucket: String, prefix: Option[String], awsClient: AmazonS3)(implicit F: Async[F]) {

  def ls(key: String, maxKeys: Int, continuationToken: Option[String]): F[S3ObjectListing] = {
    val dir = prefix.fold(key)(_ + "/" + key)

    for {
      _ <- Logger[F].info(s"Listing $dir...")
      listing <- F.pure {
        val request = {
          val req = new ListObjectsV2Request().withBucketName(bucket).withPrefix(dir)
          continuationToken.fold(req)(req.withContinuationToken(_)).withMaxKeys(maxKeys)
        }

        val objectListing = awsClient.listObjectsV2(request)
        val objectSummaries = objectListing.getObjectSummaries().asScala.toList
        val nextContinuationToken = Option(objectListing.getNextContinuationToken())
        S3ObjectListing(objectSummaries, nextContinuationToken)
      }
      _ <- Logger[F].info(s"Found ${listing.objects.length} S3 objects")
    } yield listing
  }

  def downloadFileTo(objectPath: String, destinationPath: Path): F[Unit] = {
    val key = prefix.fold(objectPath)(_ + "/" + objectPath)
    val req = new GetObjectRequest(bucket, key)
    val inputStream: InputStream = awsClient.getObject(req).getObjectContent

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

    val credentials = new BasicAWSCredentials(settings.publicKey, settings.secretKey)

    val awsClient = AmazonS3ClientBuilder.standard()
      .withRegion(settings.region)
      .withCredentials(new AWSStaticCredentialsProvider(credentials))
      .build()

    S3Client(settings.bucket, settings.prefix, awsClient)
  }
}
