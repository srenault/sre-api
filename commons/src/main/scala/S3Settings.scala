package sre.api

case class S3Settings(
  region: String,
  bucket: String,
  publicKey: String,
  secretKey: String,
  prefix: Option[String]
)
