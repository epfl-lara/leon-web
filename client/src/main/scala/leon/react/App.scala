package leon.web.client
package react

import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.scalajs.js.Dynamic.{ literal => l }

import org.scalajs.dom.document

import org.scalajs.jquery
import org.scalajs.jquery.{ jQuery => $, JQueryEventObject }
import leon.web.client.JQueryExtended._

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import monifu.concurrent.Implicits.globalScheduler

import leon.web.client.syntax.Observer._

class App(private val api: LeonAPI) {

  import leon.web.client.components._
  import leon.web.client.components.modals._
  import leon.web.client.events._
  import leon.web.client.actions._
  import leon.web.shared.{Action => LeonAction}

  private
  val appState = GlobalAppState()

  def init(): Unit = {
    Handlers.register(api.handlers)

    Actions.setActionHandler(processAction)

    appState.asObservable
      .dump("AppState")
      .foreach(render)

    Actions.register(appState.updates)
  }

  private
  def processAction(action: Action): Unit = action match {
    case LoadRepositories() =>
      val msg = l(
        action = LeonAction.loadRepositories,
        module = "main"
      )

      api.leonSocket.send(JSON.stringify(msg))

    case LoadFiles(repo) =>
      val msg = l(
        action = LeonAction.loadRepository,
        module = "main",
        owner  = repo.owner,
        name   = repo.name
      )

      api.leonSocket.send(JSON.stringify(msg))

      Actions.modState ! (_.copy(
        repository    = Some(repo),
        isLoadingRepo = true
      ))

    case LoadFile(repo, file) =>
      val msg = l(
        action = LeonAction.loadFile,
        module = "main",
        owner  = repo.owner,
        repo   = repo.name,
        file   = file
      )

      api.leonSocket.send(JSON.stringify(msg))

    case UpdateEditorCode(code) =>
      api.setEditorCode(code)
      Events.codeUpdated ! CodeUpdated()

    case ToggleLoadRepoModal(value) =>
      // nothing to do here
  }


  private
  def render(state: AppState): Unit = {
    renderLogin()
    renderLoadRepoPanel(state)
  }

  private
  def renderLogin(): Unit = {
    val el = document.getElementById("login-modal")
    ReactDOM.render(LoginModal(false), el)

    $("#login-btn").click { e: JQueryEventObject =>
      e.preventDefault()
      ReactDOM.render(LoginModal(true), el)
    }
  }

  private
  def renderLoadRepoPanel(state: AppState): Unit = {
    val panelEl = document.getElementById("load-repo-panel")

    if (panelEl != null) {
      ReactDOM.render(LoadRepositoryPanel(state), panelEl)
      $(panelEl).show()
    }
  }

}

