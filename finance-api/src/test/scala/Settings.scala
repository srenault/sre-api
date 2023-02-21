import java.nio.file._
import org.http4s.Uri
import sre.api.finance._
import sre.api.settings._

object Settings {

  private def getPathFromResource(fileName: String): Path = {
    Option(getClass().getClassLoader()).map(_.getResource(fileName)) match {
      case Some(resource) =>
        Paths.get(resource.toURI())

      case None => sys.error(s"Unable to get ${fileName} from resource")
    }
  }

  private def getResourceAbsolutePath(): String = {
    getPathFromResource("").toFile.getAbsolutePath()
  }

  private def uri(s: String): Uri = {
    Uri.fromString(s).toOption.getOrElse {
      sys.error(s"Unable to parse ${s} as Uri")
    }
  }

  def loadFinanceSettings(): FinanceSettings = {
   FinanceSettings(
      db = s"jdbc:sqlite:${getResourceAbsolutePath()}/sre.db",
            httpClient = HttpClientSettings(
        logRequest = true,
        logResponse = true
      ),
        cm = CMSettings(
          baseUri = uri("https://www.bank.com"),
          authenticationPath = "/authentification",
          validationPath = "/validation",
          homePath = "/home",
          downloadPath = "/downloads",
          transactionPath = "/transaction",
          username = "johndoe",
          password = "password",
          accounts = Nil,
          otpSession = Paths.get("/Users/test/.sre-api-otp_session"),
          apkId = "com.cm_prod.bad"
        ),
        transactionsDir = getPathFromResource("transactions"),
        wageStatements = List(
          "VIR NEW PRISMIC",
          "VIR DDFIP D INDRE ET LOIRE"
        ),
        s3 = S3Settings(
          region = "us-east-1",
          bucket = "test",
          publicKey = "xxxxx",
          secretKey = "xxxxx",
          prefix = "xxx"
        ),
        setupVolume = SetupVolumeSettings(maxKeys = 20)
      )
    
  }
}
