/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react

import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.scalajs.js.Dynamic.{ literal => l }

import org.scalajs.dom.document

import org.scalajs.jquery
import org.scalajs.jquery.{ jQuery => $, JQueryEventObject }

import japgolly.scalajs.react._

import monifu.concurrent.Implicits.globalScheduler

import leon.web.client.syntax.Observer._

/** This class is in charge of the following:
  *
  * $ - Register WebSocket handlers in order to process messages
  *     sent by the server.
  * $ - Process actions trigger by the React components.
  * $ - Holds, and tracks the application state, and trigger re-renders
  *     of the components tree when needed.
  *
  * @see [[leon.web.client.react.AppState]]
  * @see [[leon.web.client.actions.Actions]]
  * @see [[leon.web.client.events.Events]]
  */
class App(private val api: LeonAPI) {

  import leon.web.client.react.components._
  import leon.web.client.react.components.modals._
  import leon.web.shared.{Action => LeonAction}

  /** Global application state */
  private
  val appState = GlobalAppState()

  def init(): Unit = {
    // Register the WebSocket handlers.
    Handlers.register(api.handlers)

    // Set the action handler.
    Actions.setActionHandler(processAction)

    // Trigger a re-render of the app, each time
    // the application state is updated.
    appState.asObservable.foreach(render)

    // Apply every state transformation to the application state.
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

    case LoadRepository(repo) =>
      val msg = l(
        action = LeonAction.loadRepository,
        module = "main",
        owner  = repo.owner,
        repo   = repo.name
      )

      api.leonSocket.send(JSON.stringify(msg))

      Actions.modState ! (_.copy(
        repository    = Some(repo),
        branch        = Some(repo.defaultBranch),
        isLoadingRepo = true
      ))

    case SwitchBranch(repo, branch) =>
      val msg = l(
        action = LeonAction.switchBranch,
        module = "main",
        owner  = repo.owner,
        repo   = repo.name,
        branch = branch
      )

      api.leonSocket.send(JSON.stringify(msg))

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

    case SetCurrentProject(project) =>
      api.setCurrentProject(project)

      project.flatMap(_.code).foreach { code =>
        Actions.updateEditorCode ! UpdateEditorCode(code)
      }

    case _ =>
  }

  private
  def render(state: AppState): Unit = {
    renderLogin(state: AppState)
    renderLoadRepoPanel(state)
  }

  private
  def renderLogin(state: AppState): Unit = {
    val el = document.getElementById("login-modal")
    ReactDOM.render(LoginModal(state.showLoginModal), el)

    $("#login-btn").click { e: JQueryEventObject =>
      e.preventDefault()
      Actions.toggleLoginModal ! ToggleLoginModal(true)
    }
  }

  private
  def renderLoadRepoPanel(state: AppState): Unit = {
    val panelEl = document.getElementById("load-repo-panel")

    if (panelEl =!= null) {
      ReactDOM.render(LoadRepositoryPanel(state), panelEl)
      $(panelEl).show()
    }
  }

}

