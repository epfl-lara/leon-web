/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package utils

import scala.annotation.tailrec
import org.scalajs.dom.WebSocket
import shared.messages._
import scala.collection.mutable.Queue
import boopickle.Default._
import shared.messages.PicklersToServer._
import scala.scalajs.js.typedarray._

class BufferedWebSocket(val webSocket: WebSocket) extends AnyVal {

  def isOpen: Boolean =
    webSocket.readyState === WebSocket.OPEN

  def queue: Queue[ArrayBuffer] =
    BufferedWebSocket.queue

  def convert(msg: MessageToServer): ArrayBuffer = {
    new TypedArrayBufferOps(Pickle.intoBytes(msg)).arrayBuffer()
  }
    
  def sendBuffered(message: MessageToServer): Unit =
    if (isOpen) {
      BufferedWebSocket.sendAll(webSocket)
      webSocket.send(convert(message))
    }
    else {
      queue.enqueue(convert(message))
    }
}

object BufferedWebSocket {

  val queue = Queue[ArrayBuffer]()

  @tailrec
  final def sendAll(webSocket: WebSocket): Unit =
    if (!queue.isEmpty) {
      val msg = queue.dequeue()
      webSocket.send(msg)
      sendAll(webSocket)
    }

}

