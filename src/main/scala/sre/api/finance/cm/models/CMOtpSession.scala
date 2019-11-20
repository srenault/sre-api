package sre.api.finance.cm

import java.time.LocalDateTime
import cats.data.NonEmptyList
import io.circe.{ Json, Decoder, Encoder, HCursor }
import org.http4s.{ RequestCookie, ResponseCookie }

sealed trait CMOtpSession {
  def transactionId: String
  def status: CMOtpStatus
  def isPending: Boolean = status match {
    case session: CMOtpStatus.Pending => true
    case _ => false
  }
}

case class CMValidOtpSession(
  authClientStateCookie: RequestCookie,
  transactionId: String,
  requestedAt: LocalDateTime,
  validatedAt: LocalDateTime
) extends CMOtpSession {

  def status = CMOtpStatus.Validated(transactionId, validatedAt)
}

object CMValidOtpSession {
  val AUTH_CLIENT_STATE = "auth_client_state"

  def create(
    authClientStateResponseCookie: ResponseCookie,
    transactionId: String,
    requestedAt: LocalDateTime,
    validatedAt: LocalDateTime
  ): CMValidOtpSession = {
    val authClientStateCookie =
      RequestCookie(authClientStateResponseCookie.name, authClientStateResponseCookie.content)

    CMValidOtpSession(authClientStateCookie, transactionId, requestedAt, validatedAt)
  }

  implicit val decoder: Decoder[CMValidOtpSession] = new Decoder[CMValidOtpSession] {
    final def apply(c: HCursor): Decoder.Result[CMValidOtpSession] =
      for {
        key <- c.downField("key").as[String]
        value <- c.downField("value").as[String]
        transactionId <- c.downField("transactionId").as[String]
        requestedAt <- c.downField("requestedAt").as[LocalDateTime]
        validatedAt <- c.downField("validatedAt").as[LocalDateTime]
      } yield {
        val authClientStateResponseCookie = RequestCookie(key, value)
        CMValidOtpSession(authClientStateResponseCookie, transactionId, requestedAt, validatedAt)
      }
  }

  implicit val encoder: Encoder[CMValidOtpSession] = new Encoder[CMValidOtpSession] {
    final def apply(otpSession: CMValidOtpSession): Json = {
      Json.obj(
        "key" -> Json.fromString(otpSession.authClientStateCookie.name),
        "value" -> Json.fromString(otpSession.authClientStateCookie.content),
        "transactionId" -> Json.fromString(otpSession.transactionId),
        "requestedAt" -> Json.fromString(otpSession.requestedAt.toString),
        "validatedAt" -> Json.fromString(otpSession.validatedAt.toString)
      )
    }
  }
}

case class CMPendingOtpSession(
  otpHidden: String,
  inputHiddenInAppsEndNew1: String,
  globalBackup: String,
  transactionId: String,
  otpAuthCookie: RequestCookie,
  otherCookies: List[RequestCookie],
  requestedAt: LocalDateTime,
) extends CMOtpSession {

  def cookies: NonEmptyList[RequestCookie] = NonEmptyList(otpAuthCookie, otherCookies)

  def status = CMOtpStatus.Pending(transactionId, requestedAt)

  def toOtpRequest(apkId: String) = CMOtpRequest(transactionId, requestedAt, apkId)

  def validate(cookie: ResponseCookie): CMValidOtpSession =
    CMValidOtpSession.create(
      authClientStateResponseCookie = cookie,
      transactionId = transactionId,
      requestedAt = requestedAt,
      validatedAt = LocalDateTime.now
    )
}

object CMPendingOtpSession {
  val OTPAUTH_COOKIE = "%5Fwirt%5Fsosddmzsiwfcmcicfr%5FSOSD%5FOTPAUTH%5Fsaguid"
  val OTP_HIDDEN_FIELD_ID = "otp_hidden"
  val GLOBAL_BACKUP_FIELD_ID = "global_backup_hidden_key"
  val INPUT_HIDDEN_KEY_IN_APPS_END_NEW1_ID = "InputHiddenKeyInAppSendNew1"
  val FID_DO_VALIDATE_X_FIELD = "_FID_DoValidate.x" -> "0"
  val FID_DO_VALIDATE_Y_FIELD = "_FID_DoValidate.y" -> "0"
  val WXF2_CC_FIELD = "_wxf2_cc" -> "fr-FR"

  def create(
    otpHidden: String,
    inputHiddenInAppsEndNew1: String,
    globalBackup: String,
    transactionId: String,
    otpAuthCookie: ResponseCookie,
    otherCookies: List[ResponseCookie]
  ): CMPendingOtpSession = {
    CMPendingOtpSession(
      otpHidden,
      inputHiddenInAppsEndNew1,
      globalBackup,
      transactionId,
      otpAuthCookie = RequestCookie(otpAuthCookie.name, otpAuthCookie.content),
      otherCookies = otherCookies.map( c => RequestCookie(c.name, c.content)),
      requestedAt = LocalDateTime.now
    )
  }
}
