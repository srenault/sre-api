package sre.api

import org.http4s.Uri
import java.io.File
import java.nio.file.{ Path, Paths }
import io.circe._
import io.circe.parser._
import io.circe.generic.auto._

object Env {

  lazy val env = System.getenv

  def getString(key: String): Option[String] =
    Option(env.get(key))

  def getStringOrFail(key: String): String =
    getString(key) getOrElse sys.error(s"Configuration error: Unable to get $key")

  def getIntOrFail(key: String): Int =
    scala.util.Try(getStringOrFail(key).toInt) getOrElse {
      sys.error(s"Configuration error: Unable to get $key as Int")
    }

  def getUri(key: String): Option[Uri] =
    getString(key).flatMap(Uri.fromString(_).toOption)

  def getUriOrFail(key: String): Uri =
    getUri(key) getOrElse sys.error(s"Configuration error: Unable to get $key")

  def getBoolean(key: String): Option[Boolean] =
    scala.util.Try(getStringOrFail(key).toBoolean).toOption

  def getBooleanOrFail(key: String): Boolean =
    getBoolean(key) getOrElse {
      sys.error(s"Configuration error: Unable to get $key as Boolean")
    }

  def getFileOrFail(key: String): File = {
    val path = getString(key) getOrElse {
      sys.error(s"Configuration error: Unable to get $key as File, not found")
    }

    val file = new File(path)

    if (!file.exists) {
      sys.error(s"Configuration error: Unable to get $key as file, $path not found")
    } else {
      file
    }
  }

  def getPathOrFail(key: String): Path = {
    val src = getString(key) getOrElse {
      sys.error(s"Configuration error: Unable to get $key as Path, not found")
    }

    val path = Paths.get(src)

    if (!path.toFile.exists) {
      sys.error(s"Configuration error: Unable to get $key as Path, $path not found")
    } else {
      path
    }
  }

  def getJsonAsOrFail[A : Decoder](key: String): A = {
    val s = getString(key) getOrElse {
      sys.error(s"Configuration error: Unable to get $key as Json, not found")
    }

    val res = for {
      json <- parse(s)
      value <- json.as[A]
    } yield value

    res.getOrElse {
      sys.error(s"Configuration error: Unable to get $key as Json: $s")
    }
  }

  private def update(update: java.util.Map[String, String] => Unit) = {
    val field = env.getClass().getDeclaredField("m")
    field.setAccessible(true)
    val r = field.get(env).asInstanceOf[java.util.Map[String, String]]
    update(r)
  }

  def set(name: String, value: String) = {
    update(_.put(name, value))
  }

  def del(name: String): Unit = {
    update(_.remove(name))
  }

  lazy val AWS_LAMBDA_FUNCTION_NAME = getStringOrFail("AWS_LAMBDA_FUNCTION_NAME")
  lazy val AWS_LAMBDA_FUNCTION_VERSION = getStringOrFail("AWS_LAMBDA_FUNCTION_VERSION")
  lazy val AWS_LAMBDA_FUNCTION_MEMORY_SIZE = getIntOrFail("AWS_LAMBDA_FUNCTION_MEMORY_SIZE")
  lazy val AWS_LAMBDA_LOG_GROUP_NAME = getStringOrFail("AWS_LAMBDA_LOG_GROUP_NAME")
  lazy val AWS_LAMBDA_LOG_STREAM_NAME = getStringOrFail("AWS_LAMBDA_LOG_STREAM_NAME")
}
