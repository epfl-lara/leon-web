/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react

import scala.scalajs.js
import monifu.concurrent.Implicits.globalScheduler

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
    handlers += ("git_operation_done"  -> gitOperationDoneHandler)
  }

  val reposHandler = (data: HRepositories) => {
    Events.repositoriesLoaded onNext RepositoriesLoaded(data.repos)
  }

  val loadRepoHandler = (data: HRepositoryLoaded) => {
    Events.repositoryLoaded onNext RepositoryLoaded(data.repository, data.files, data.branches, data.currentBranch)
  }

  val loadFileHandler = (data: HFileLoaded) => {
    Events.fileLoaded onNext FileLoaded(data.file, data.content)
  }

  val changeBranchHandler = (data: HBranchChanged) => {
    if (data.success)
      Events.branchChanged onNext BranchChanged(data.branch.get, data.files.get)
  }

  val gitProgressHandler = (data: HGitProgress) => {
    Events.gitProgress onNext GitProgress(data.taskName, data.percentage.toOption)
  }

  val gitOperationDoneHandler = (data: HGitOperationResult) => {
    Events.gitOperationDone onNext GitOperationDone(data)
  }

}

