/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package workers

import akka.actor._
import play.api.libs.json._
import play.api.libs.json.Json._

import leon.web.models.{BaseActor, ConsoleProtocol}

/** This worker sends back to the client progress updates for JGit
  * operations, and is meant to be used together with a
  * [[leon.web.models.JGitProgressMonitor]].
  *
  * @param eventName the name of the event to send back to the client
  * @param session   a ref to an actor able to handle
  *                  [[leon.web.models.ConsoleProtocol.NotifyClient]] messages.
  *
  * @see [[leon.web.models.ConsoleSession]]
  */
class JGitProgressWorker(eventName: String, session: ActorRef) extends BaseActor {

  import ConsoleProtocol._

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

