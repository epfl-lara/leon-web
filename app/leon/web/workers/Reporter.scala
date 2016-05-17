package leon
package web
package workers

import akka.actor._
import play.api._

import models._

class WorkerReporter(session: ActorRef) extends Reporter(Set()) {
  import ConsoleProtocol.NotifyClient

  def emit(msg: Message) = {
    val prefix = msg.severity match {
      case INFO => ""
      case WARNING => "Warning: "
      case ERROR => "Warning: "
      case FATAL => "Fatal: "
      case DEBUG(_) => "Debug: "
    }

    session ! NotifyClient(shared.messages.HLog(message = (prefix + msg.msg.toString)))
    println((prefix + msg.msg.toString))
  }
}
