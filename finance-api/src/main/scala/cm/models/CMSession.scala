package sre.api.finance.cm

case class CMSession(
    basicAuthSession: CMBasicAuthSession,
    otpSession: CMOtpSession
)
