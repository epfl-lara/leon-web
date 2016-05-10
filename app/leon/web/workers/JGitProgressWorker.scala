/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package workers

import akka.actor._
import play.api.libs.json._
import play.api.libs.json.Json._
import leon.web.models.{BaseActor, ConsoleProtocol}
import leon.web.shared.messages.GitProgress

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
      event(GitProgress(taskName, "update", percentage.map(_.toString).orElse(Some("?"))))

    case OnJGitProgressEnd(taskName, _, _, _) =>
      event(GitProgress(taskName, "end", None))

    case _ =>
  }

  def pushMessage(v: Array[Byte]): Unit = session ! NotifyClientBin(v)

}

