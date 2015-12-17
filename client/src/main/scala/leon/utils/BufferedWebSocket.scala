package leon.web
package client
package utils

import scala.annotation.tailrec

import org.scalajs.dom.WebSocket

import scala.collection.mutable.Queue

class BufferedWebSocket(val webSocket: WebSocket) extends AnyVal {

  def isOpen: Boolean =
    webSocket.readyState == WebSocket.OPEN

  def queue: Queue[String] =
    BufferedWebSocket.queue

  def sendBuffered(message: String): Unit =
    if (isOpen) {
      BufferedWebSocket.sendAll(webSocket)
      webSocket.send(message)
    }
    else {
      queue.enqueue(message)
    }

}

object BufferedWebSocket {

  val queue = Queue[String]()

  @tailrec
  final def sendAll(webSocket: WebSocket): Unit =
    if (!queue.isEmpty) {
      val msg = queue.dequeue()
      webSocket.send(msg)
      sendAll(webSocket)
    }

}

