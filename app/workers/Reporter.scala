package leon
package web
package workers

import akka.actor._
import play.api.libs.json._
import play.api.libs.json.Json._

import models._

class WorkerReporter(session: ActorRef) extends Reporter(Settings()) {
  import ConsoleProtocol.NotifyClient

  def emit(msg: Message) = {
    val prefix = msg.severity match {
      case INFO => ""
      case WARNING => "Warning: "
      case ERROR => "Warning: "
      case FATAL => "Fatal: "
      case DEBUG(_) => "Debug: "
    }

    session ! NotifyClient(toJson(Map("kind" -> "log", "message" -> (prefix + msg.msg.toString))))
  }
}
