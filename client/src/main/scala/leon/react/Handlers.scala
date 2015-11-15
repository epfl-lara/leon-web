/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web.client
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
    handlers += ("repositories"    -> reposHandler)
    handlers += ("load_repository" -> loadRepoHandler)
    handlers += ("load_file"       -> loadFileHandler)
    handlers += ("git_progress"    -> gitProgressHandler)
  }

  val reposHandler = (data: HRepositories) => {
    Events.repositoriesLoaded ! RepositoriesLoaded(data.repos)
  }

  val loadRepoHandler = (data: HLoadRepository) => {
    Events.filesLoaded ! FilesLoaded(data.files)
  }

  val loadFileHandler = (data: HLoadFile) => {
    Events.fileLoaded ! FileLoaded(data.file, data.content)
  }

  val gitProgressHandler = (data: HGitProgress) => {
    Events.gitProgress ! GitProgress(data.taskName, data.percentage.toOption)
  }

}

