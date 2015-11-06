package leon.web
package workers

import akka.actor._
import play.api.libs.json._
import play.api.libs.json.Json._

import leon.web.models.{BaseActor, ConsoleProtocol}

class JGitProgressWorker(eventName: String, session: ActorRef) extends BaseActor {

  import ConsoleProtocol._

  import context._

  def receive = {
    case OnJGitProgressUpdate(taskName, _, _, percentage) =>
      event(eventName, Map(
        "taskName"   -> toJson(taskName),
        "status"     -> toJson("update"),
        "percentage" -> toJson(percentage.map(_.toString).getOrElse("?"))
      ))

    case OnJGitProgressEnd(taskName, _, _, _) =>
      event(eventName, Map(
        "taskName"   -> toJson(taskName),
        "status"     -> toJson("end")
      ))

    case _ =>
  }

  def pushMessage(v: JsValue): Unit = session ! NotifyClient(v)

}

