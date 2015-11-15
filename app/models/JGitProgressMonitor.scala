/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package models

import akka.actor._
import org.eclipse.jgit.lib.BatchingProgressMonitor

/** JGit progress monitor that forwards progress updates to the
  * specified actor, typically a [[leon.web.workers.JGitProgressWorker]].
  *
  * @param actor The actor to forward the progress updates to.
  */
class JGitProgressMonitor(actor: ActorRef) extends BatchingProgressMonitor {

  import ConsoleProtocol._

  override
  protected def onUpdate(taskName: String, curWork: Int, totalWork: Int, pcnt: Int): Unit = {
    actor ! OnJGitProgressUpdate(taskName, curWork, Some(totalWork), Some(pcnt))
  }

  override
  protected def onUpdate(taskName: String, curWork: Int): Unit = {
    actor ! OnJGitProgressUpdate(taskName, curWork)
  }

  override
  protected def onEndTask(taskName: String, curWork: Int, totalWork: Int, pcnt: Int): Unit = {
    actor ! OnJGitProgressEnd(taskName, curWork, Some(totalWork), Some(pcnt))
  }

  override
  protected def onEndTask(taskName: String, workCur: Int): Unit = {
    actor ! OnJGitProgressEnd(taskName, workCur)
  }

}

