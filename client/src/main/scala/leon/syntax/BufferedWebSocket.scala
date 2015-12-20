/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package syntax

import org.scalajs.dom.WebSocket

import leon.web.client.utils.BufferedWebSocket

object BufferedWebSocketOps {

  implicit def toBufferedWebSocket(webSocket: WebSocket): BufferedWebSocket =
    new BufferedWebSocket(webSocket)

}

