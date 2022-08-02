package sre.api

import com.amazonaws.auth.{ BasicAWSCredentials, AWSStaticCredentialsProvider }
import com.amazonaws.services.s3.model._
import com.amazonaws.services.s3.{ AmazonS3ClientBuilder, AmazonS3 }
import scala.jdk.CollectionConverters._
import cats.effect._
import fs2.Stream

case class S3ObjectListing(objects: List[S3ObjectSummary], continuationToken: Option[String])

case class S3Client[F[_]](bucket: String, prefix: Option[String], awsClient: AmazonS3)(implicit F: Async[F]) {

  def ls(key: String, continuationToken: Option[String]): F[S3ObjectListing] = F.pure {
    val p = prefix.fold(key)(_ + "/" + key)
    val request = {
      val req = new ListObjectsV2Request().withBucketName(bucket).withPrefix(p)
      continuationToken.fold(req)(req.withContinuationToken(_))
    }

    val objectListing = awsClient.listObjectsV2(request)
    val objectSummaries = objectListing.getObjectSummaries().asScala.toList
    val nextContinuationToken = Option(objectListing.getNextContinuationToken())
    S3ObjectListing(objectSummaries, nextContinuationToken)
  }

  def streamFile(file: String): Stream[F, Byte] = {
    val key = prefix.fold(file)(_ + "/" + file)
    val req = new GetObjectRequest(bucket, key)
    val inputStream: java.io.InputStream = awsClient.getObject(req).getObjectContent
    fs2.io.readInputStream(F.pure(inputStream), 1024)
  }
}


object S3Client {

  def apply[F[_]: Async](settings: S3Settings): S3Client[F] = {
    val credentials = new BasicAWSCredentials(settings.publicKey, settings.secretKey)

    val awsClient = AmazonS3ClientBuilder.standard()
      .withRegion(settings.region)
      .withCredentials(new AWSStaticCredentialsProvider(credentials))
      .build()

    S3Client(settings.bucket, settings.prefix, awsClient)
  }
}
