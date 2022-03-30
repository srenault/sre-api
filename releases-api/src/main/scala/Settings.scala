package sre.api.releases

import org.http4s.Uri

case class Settings(
  s3: S3Settings,
  advertisedAddress: Uri
)

object Settings {

  lazy val env = System.getenv

  private def getString(key: String): Option[String] =
    Option(env.get(key))

  private def getStringOrFail(key: String): String =
    getString(key) getOrElse sys.error(s"Configuration error: Unable to get $key")

  private def getUri(key: String): Option[Uri] =
    getString(key).flatMap(Uri.fromString(_).toOption)

  private def getUriOrFail(key: String): Uri =
    getUri(key) getOrElse sys.error(s"Configuration error: Unable to get $key")

  private def getBoolean(key: String): Option[Boolean] =
    scala.util.Try(getStringOrFail(key).toBoolean).toOption

  private def getBooleanOrFail(key: String): Boolean =
    getBoolean(key) getOrElse {
      sys.error(s"Configuration error: Unable to get $key as Boolean")
    }


  def build(): Settings = {
    Settings(
      advertisedAddress = getUriOrFail("releases.advertisedAddress"),
      s3 = S3Settings(
        region = getStringOrFail("releases.s3.region"),
        bucket = getStringOrFail("releases.s3.bucket"),
        publicKey = getStringOrFail("releases.s3.publickey"),
        secretKey = getStringOrFail("releases.s3.secretkey"),
        prefix = getString("releases.s3.prefix")
      )
    )
  }
}

case class S3Settings(
  region: String,
  bucket: String,
  publicKey: String,
  secretKey: String,
  prefix: Option[String]
)
