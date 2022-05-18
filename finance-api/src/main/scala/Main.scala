package sre.api
package finance

import com.amazonaws.services.lambda.runtime._
import java.io._
import java.nio.file.{ Paths, Path }
import cats.effect._
import cats.effect.std.Random
import fs2.text
import fs2.io.file.{Files => FS2Files, Path => FS2Path}

object Main extends IOApp {

  private def buildAwsContext(): IO[Context] = {
    for {
      rnd <- Random.scalaUtilRandom[IO]
      awsRequestId <- rnd.nextString(18)
    } yield {
      new Context {
        def getAwsRequestId(): String = awsRequestId
        def getLogGroupName(): String = Env.AWS_LAMBDA_LOG_GROUP_NAME
        def getLogStreamName(): String = Env.AWS_LAMBDA_LOG_STREAM_NAME
        def getFunctionName(): String = Env.AWS_LAMBDA_FUNCTION_NAME
        def getFunctionVersion(): String = Env.AWS_LAMBDA_FUNCTION_VERSION
        def getInvokedFunctionArn(): String = "invokedFunctionArn"
        def getIdentity(): CognitoIdentity = null
        def getClientContext(): ClientContext = null
        def getRemainingTimeInMillis(): Int = (1000 - System.currentTimeMillis).toInt
        def getMemoryLimitInMB(): Int = Env.AWS_LAMBDA_FUNCTION_MEMORY_SIZE
        def getLogger(): LambdaLogger = new LambdaLogger() {
          def log(s: String) = {
            println(s)
          }

          def log(s: Array[Byte]) = {
            println(new String(s, "UTF-8"))
          }
        }
      }
    }
  }

  def printUsage(): IO[Unit] = IO.delay {
    println("finance-api --file <path-to-event>")
  }

  private def parseArgs(args: List[String]): Map[Symbol, String] = {
    @annotation.tailrec
    def step(list: List[String], acc: Map[Symbol, String] = Map.empty): Map[Symbol, String] = {
      list match {
        case "--file" :: value :: tail =>
          step(tail, acc + ('file -> value))

        case _ => acc
      }
    }

    step(args)
  }

  private def validateArgs[A](args: List[String])(success: Path => IO[A])(failure: IO[A]): IO[A] = {
    val parsedArgs = parseArgs(args)
    (for {
      eventPath <- parsedArgs.get('file).map(Paths.get(_))
      eventFile = eventPath.toFile
      if !eventFile.isDirectory && eventFile.exists
    } yield {
      success(eventPath)
    }) getOrElse failure
  }

  private def buildInputStream(eventPath: Path): Resource[IO, InputStream] = {
    val eventuallyContent = FS2Files[IO]
      .readAll(FS2Path(eventPath.toString))
      .through(text.utf8Decode)
      .through(text.lines)
      .compile.fold("")(_ +  _)

    Resource.fromAutoCloseable(eventuallyContent.map { content =>
      new ByteArrayInputStream(content.getBytes("UTF-8"))
    })
  }

  private def buildOutputStream(): Resource[IO, OutputStream] =
    Resource.fromAutoCloseable(IO.blocking(new ByteArrayOutputStream()))


  override def run(args: List[String]): IO[ExitCode] = {
    validateArgs(args) {
      case eventPath =>
        buildAwsContext().flatMap { awsContext =>
          (
            for {
              is <- buildInputStream(eventPath)
              os <- buildOutputStream()
            } yield (is, os)
          ).use {
            case (is, os) =>
              IO.blocking(Handler.handleRequest(is, os, awsContext)).map(_ => ())
          }
        }
    }(printUsage()).as(ExitCode.Success)
  }
}
