package sre.api
package utils

import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.s3.model._
import com.amazonaws.services.s3.AmazonS3Client
import scala.collection.JavaConverters._
import cats.effect._
import fs2.Stream

case class S3Client[F[_]](bucket: String, prefix: Option[String], awsClient: AmazonS3Client)(implicit F: Effect[F], cs: ContextShift[F]) {

  def ls(key: String = ""): F[List[S3ObjectSummary]] = F.pure {
    val p = prefix.fold(key)(_ + "/" + key)
    val req = new ListObjectsRequest().withBucketName(bucket).withPrefix(p)

    @annotation.tailrec
    def loop(current: ObjectListing, acc: List[S3ObjectSummary]): List[S3ObjectSummary] = {
      if (!current.isTruncated) acc  else {
        val nextPage = awsClient.listNextBatchOfObjects(current)
        val keys = nextPage.getObjectSummaries().asScala.toList
        loop(nextPage, acc ++: keys)
      }
    }

    val firstPage = awsClient.listObjects(req)

    loop(firstPage, acc = firstPage.getObjectSummaries.asScala.toList)
  }

  def streamFile(file: String): Stream[F, Byte] = {
    val key = prefix.fold(file)(_ + "/" + file)
    val req = new GetObjectRequest(bucket, key)
    val inputStream: java.io.InputStream = awsClient.getObject(req).getObjectContent
    val blockingExecutionContext = scala.concurrent.ExecutionContext.global
    fs2.io.readInputStream(F.pure(inputStream), 1024, blockingExecutionContext)
  }
}

object S3Client {

  def apply[F[_]: Effect](settings: S3Settings)(implicit cs: ContextShift[F]): S3Client[F] = {
    val credentials = new BasicAWSCredentials(settings.publicKey, settings.secretKey)
    val awsClient = new AmazonS3Client(credentials)
    S3Client(settings.bucket, settings.prefix, awsClient)
  }
}