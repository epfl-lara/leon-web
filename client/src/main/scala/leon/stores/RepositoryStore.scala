package leon.web.client
package stores

import scala.scalajs.js
import scala.scalajs.js.JSON
import org.scalajs.dom.{ document, console }
import scala.scalajs.js.Dynamic.{ literal => l }
import org.scalajs.jquery

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import leon.web.shared.Action
import leon.web.client.HandlersTypes._

object RepositoryStore {

  sealed trait Action
  case class LoadRepositories() extends Action
  case class LoadFiles(repo: HRepository) extends Action
  case class LoadFile(repo: HRepository, file: String) extends Action
  case class SetEditorCode(code: String) extends Action

  sealed trait Event
  case class RepositoriesLoaded(repos: Seq[HRepository]) extends Event
  case class FilesLoaded(files: Seq[String]) extends Event
  case class FileLoaded(fileName: String, content: String) extends Event

  type Listener = Event => Unit

  private var listeners: List[Listener] = List()

  private var api: LeonAPI = _

  def init(leonAPI: LeonAPI): Unit = {
    api = leonAPI

    api.handlers += ("repositories"    -> repoHandler)
    api.handlers += ("load_repository" -> loadRepoHandler)
    api.handlers += ("load_file"       -> loadFileHandler)
  }

  def listen(listener: Listener): Unit =
    listeners = listener :: listeners

  def emit(event: Event): Unit = {
    listeners.foreach(_(event))
  }

  def processAction(action: Action): Unit = action match {
    case LoadRepositories() =>
      val msg = l(
        action = Action.loadRepositories,
        module = "main"
      )

      api.leonSocket.send(JSON.stringify(msg))

    case LoadFiles(repo) =>
      val msg = l(
        action = Action.loadRepository,
        module = "main",
        owner  = repo.owner,
        name   = repo.name
      )

      api.leonSocket.send(JSON.stringify(msg))

    case LoadFile(repo, file) =>
      val msg = l(
        action = Action.loadFile,
        module = "main",
        owner  = repo.owner,
        repo   = repo.name,
        file   = file
      )

      api.leonSocket.send(JSON.stringify(msg))

    case SetEditorCode(code) =>
      api.setEditorCode(code)
  }

  def !(action: Action): Unit = processAction(action)

  private
  val repoHandler = (data: HRepositories) => {
    if (data.status == "error") {
      console.error(data.error)
    }

    emit(RepositoriesLoaded(data.repos))
  }

  private
  val loadRepoHandler = (data: HLoadRepository) => {
    if (data.status == "error") {
      console.error(data.error)
    }

    emit(FilesLoaded(data.files))
  }

  private
  val loadFileHandler = (data: HLoadFile) => {
    if (data.status == "error") {
      console.error(data.error)
    }

    emit(FileLoaded(data.file, data.content))
  }

}

