package leon
package web
package workers

import akka.actor._
import play.api.libs.json._
import play.api.libs.json.Json._

import models._

class WorkerReporter(session: ActorRef) extends Reporter(Settings()) {
  import ConsoleProtocol.NotifyClient

  def infoFunction(msg: Any) : Unit = {
    session ! NotifyClient(toJson(Map("kind" -> "log", "message" -> (msg.toString))))
  }

  def warningFunction(msg: Any) : Unit = {
    session ! NotifyClient(toJson(Map("kind" -> "log", "message" -> ("Warning: "+msg.toString))))
  }

  def errorFunction(msg: Any) : Unit = {
    session ! NotifyClient(toJson(Map("kind" -> "log", "message" -> ("Error: "+msg.toString))))
  }

  def debugFunction(msg: Any) : Unit = {
    session ! NotifyClient(toJson(Map("kind" -> "log", "message" -> ("Debug: "+msg.toString))))
  }

  def fatalErrorFunction(msg: Any) : Nothing = {
    sys.error("FATAL: "+msg)
  }
}
