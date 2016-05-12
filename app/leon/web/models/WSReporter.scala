package leon.web
package models

import play.api.libs.iteratee._

import leon.Reporter

class WSReporter(channel: Concurrent.Channel[Array[Byte]]) extends Reporter(Set()) {
  def emit(msg: Message) = {
    val prefix = msg.severity match {
      case INFO => ""
      case WARNING => "Warning: "
      case ERROR => "Warning: "
      case FATAL => "Fatal: "
      case DEBUG(_) => "Debug: "
    }

    import boopickle.Default._
    import shared.messages.{MessageFromServer, HLog}
    import shared.messages.MessageFromServer._
    channel.push(Pickle.intoBytes[MessageFromServer](HLog(prefix + msg.msg.toString)).array())
  }
}
