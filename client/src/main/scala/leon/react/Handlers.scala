/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react

import scala.scalajs.js
import monifu.concurrent.Implicits.globalScheduler

import leon.web.client.syntax.Observer._
import leon.web.client.HandlersTypes._

/** Register WebSocket handlers, and push the received messages
  * through the appropriate event bus.
  *
  * @see [[leon.web.client.events.Event]]
  */
object Handlers {

  /** Register the handlers, by adding them to the specified
    * mutable dictionary.
    *
    * @see [[leon.web.client.Main.handlers]]
    */
  def register(handlers: js.Dictionary[Any]): Unit = {
    handlers += ("repositories_loaded" -> reposHandler)
    handlers += ("repository_loaded"   -> loadRepoHandler)
    handlers += ("file_loaded"         -> loadFileHandler)
    handlers += ("branch_changed"      -> changeBranchHandler)
    handlers += ("git_progress"        -> gitProgressHandler)
  }

  val reposHandler = (data: HRepositories) => {
    Events.repositoriesLoaded ! RepositoriesLoaded(data.repos)
  }

  val loadRepoHandler = (data: HRepositoryLoaded) => {
    Events.repositoryLoaded ! RepositoryLoaded(data.repository, data.files, data.branches)
  }

  val loadFileHandler = (data: HFileLoaded) => {
    Events.fileLoaded ! FileLoaded(data.file, data.content)
  }

  val changeBranchHandler = (data: HBranchChanged) => {
    Events.branchChanged ! BranchChanged(data.branch, data.files)
  }

  val gitProgressHandler = (data: HGitProgress) => {
    Events.gitProgress ! GitProgress(data.taskName, data.percentage.toOption)
  }

}

