package sre.api
package domoticz
package websocket

import scala.concurrent.duration._
import org.slf4j.LoggerFactory
import java.util.Base64
import java.net.URI
import org.java_websocket.client.{WebSocketClient => JavaWebSocketClient}
import org.java_websocket.protocols._
import org.java_websocket.handshake.ServerHandshake
import org.java_websocket.drafts._
import scala.jdk.CollectionConverters._
import org.java_websocket.extensions._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._
import io.circe.parser._
import fs2.Stream
import messages.WebSocketMessage

trait WebSocketListener {
  def onMessage(message: WebSocketMessage): Unit
}

case class WebSocketClient[F[_]](settings: DomoticzSettings)(listener: WebSocketListener)(implicit F: ConcurrentEffect[F], T: Timer[F]) {

  self =>

  val logger = LoggerFactory.getLogger("sre.api.domoticz.websocket.WebSocketClient")

  private val uri = new URI(settings.wsUri.renderString)

  private val authorizationHeader = {
    val value = s"${settings.username}:${settings.password}"
    val encodedValue = Base64.getEncoder().encodeToString(value.getBytes("UTF-8"))
    "Authorization" -> s"Basic $encodedValue"
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

  val innerClientRef = Ref.of[F, Option[JavaWebSocketClient]](None)

  def createInnerClient(): JavaWebSocketClient = {
    new JavaWebSocketClient(uri, draft,  headers.asJava) {
      override def onOpen(handshakedata: ServerHandshake): Unit = {
        logger.info("Domoticz websocket connection is opened")
      }

      override def onClose(code: Int, reason: String, remote: Boolean): Unit = {
        logger.info(s"Domoticz websocket connection has been closed:\n${reason}\n Trying to reopen it...")
        val retryStream = Stream.awakeDelay[F](5.seconds).zipRight(Stream.eval(self.connect()))
         F.runAsync(retryStream.compile.drain)(_ => IO.unit).unsafeRunSync
      }

      override def onMessage(message: String): Unit = {
        parse(message).flatMap(_.as[WebSocketMessage]) match {
          case Right(message) =>
            listener.onMessage(message)

          case Left(error) =>
            logger.warn(s"Unable to parse domoticz event from:\n${message}\n${error}")
        }
      }

      override def onError(ex: Exception): Unit = {
        logger.error(s"Domoticz websocket connection error: ${ex.getMessage}")
        ex.printStackTrace
      }
    }
  }

  def connect(): F[Unit] = {
    innerClientRef.flatMap { ref =>
      ref.update {
        case Some(innerClient) if innerClient.isOpen =>
          Some(innerClient)

        case _ =>
          val innerClient = createInnerClient()
          innerClient.connect()
          Some(innerClient)
      }
    }
  }
}
