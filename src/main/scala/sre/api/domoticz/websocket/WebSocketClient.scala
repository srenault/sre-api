package sre.api
package domoticz
package websocket

import java.util.Base64
import java.net.URI
import org.java_websocket.client.{WebSocketClient => JavaWebSocketClient}
import org.java_websocket.protocols._
import org.java_websocket.handshake.ServerHandshake
import org.java_websocket.drafts._
import scala.collection.JavaConverters._
import org.java_websocket.extensions._
import cats.effect._

trait WebSocketListener {
  def onOpen(): Unit
  def onMessage(message: String): Unit
  def onClose(code: Int, reason: String, remote: Boolean): Unit
  def onError(ex: Exception): Unit
}

case class WebSocketClient[F[_]](settings: DomoticzSettings)(listener: WebSocketListener)(implicit F: Sync[F]) {
  private val uri = new URI(settings.wsUri.renderString)

  private val authorizationHeader = {
    val value = s"${settings.username}:${settings.password}"
    val encodedValue = Base64.getEncoder().encodeToString(value.getBytes("UTF-8"))
    "Authorization" -> s"Basic $encodedValue",
    }

  private val originHeader = {
    val hostUri = s"${uri.getScheme}://${uri.getHost}"
    "Origin" -> hostUri
  }

  private val headers = Map(
    originHeader,
    authorizationHeader
  )

  private val extensions = List.empty[IExtension]

  private val protocols: List[IProtocol] = List(new Protocol("domoticz"))

  private val draft = new Draft_6455(extensions.asJava, protocols.asJava)

  private val innerClient = new JavaWebSocketClient(uri, draft,  headers.asJava) {
    override def onOpen(handshakedata: ServerHandshake) {
      listener.onOpen()
    }

    override def onClose(code: Int, reason: String, remote: Boolean) {
      listener.onClose(code, reason, remote)
    }

    override def onMessage(message: String) {
      listener.onMessage(message)
    }

    override def onError(ex: Exception) {
      listener.onError(ex)
    }
  }

  def isOpen: Boolean = innerClient.isOpen

  def connect(): F[Unit] = {
    Sync[F].delay {
      innerClient.connect()
    }
  }
}
