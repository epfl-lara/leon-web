package leon.web.client
package react

import scala.scalajs.js
import monifu.concurrent.Implicits.globalScheduler

import leon.web.client.syntax.Observer._
import leon.web.client.events._
import leon.web.client.HandlersTypes._

object Handlers {

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

