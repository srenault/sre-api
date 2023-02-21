package sre.api.utils

import java.security.MessageDigest
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import java.util.Base64

object Security {

  def hmacSha256(value: String, secret: String): String = {
    val secretKeySpec = new SecretKeySpec(secret.getBytes("UTF-8"), secret)
    val hmacsha256 = Mac.getInstance("HmacSHA256")
    hmacsha256.init(secretKeySpec)
    val signed = hmacsha256.doFinal(value.getBytes("UTF-8"))
    Base64.getEncoder().encodeToString(signed)
  }

  def sha1(value: String): String = {
    val messageDigest = MessageDigest.getInstance("SHA-1");
    messageDigest.update(value.getBytes("UTF-8"))
    Base64.getEncoder().encodeToString(messageDigest.digest())
  }

  def md5(value: String): String = {
    val messageDigest = MessageDigest.getInstance("MD5")
    val digest = messageDigest.digest(value.getBytes("UTF-8"))
    Base64.getEncoder().encodeToString(digest)
  }
}
