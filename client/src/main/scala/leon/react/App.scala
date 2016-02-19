/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react

import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.scalajs.js.Dynamic.{ literal => l, global => g }

import org.scalajs.dom.ext.LocalStorage
import org.scalajs.dom.{console, document}

import org.scalajs.jquery
import org.scalajs.jquery.{ jQuery => $, JQueryEventObject }

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import monifu.concurrent.Implicits.globalScheduler

import leon.web.client.syntax.observer._
import leon.web.client.syntax.websocket._

import leon.web.shared.GitOperation

/** This class is in charge of the following:
  *
  * $ - Register WebSocket handlers in order to process messages
  *     sent by the server.
  * $ - Process actions trigger by the React components.
  * $ - Holds, tracks and restore the application state, then trigger re-renders
  *     of the components tree when needed.
  *
  * @see [[leon.web.client.react.AppState]]
  * @see [[leon.web.client.actions.Actions]]
  * @see [[leon.web.client.events.Events]]
  */
class App(private val api: LeonAPI) {

  import leon.web.client.react.components._
  import leon.web.client.react.components.modals._
  import leon.web.client.react.components.panels._

  import leon.web.shared.{Action => LeonAction}

  lazy val isLoggedIn = g._leon_isLoggedIn.asInstanceOf[Boolean]

  def init(): Unit = {
    // Register the WebSocket handlers.
    Handlers.register(api.handlers)

    // Set the action handler.
    Actions.setActionHandler(processAction)

    val appState =
      LocalStorage("appState")
        .map(AppState.fromJSON)
        .map(resetAppState)
        .map(GlobalAppState(_))
        .getOrElse(GlobalAppState())

    // Trigger a re-render of the app, each time
    // the application state is updated.
    appState
      .asObservable
      .doWork(onStateUpdate)
      .foreach(render)

    // Apply every state transformation to the application state.
    Actions.register(appState.updates)

    // If the user is logged-in and was working on a project,
    // restore such project.
    if (isLoggedIn) {
      restoreAppState(appState.initial)
    }
  }

  private
  def resetAppState(state: AppState): AppState = state.copy(
    isLoggedIn     = isLoggedIn,
    showLoginModal = state.showLoginModal && !isLoggedIn,
    repository     = if (isLoggedIn) state.repository else None,
    branch         = if (isLoggedIn) state.branch     else None,
    file           = if (isLoggedIn) state.file       else None,
    isLoadingRepo  = false
  )

  private
  def restoreAppState(state: AppState): Unit = {
    println("Restoring application state...")

    api.setCurrentProject(state.currentProject)
    Actions.setTreatAsProject ! SetTreatAsProject(state.treatAsProject)
  }

  private
  def onStateUpdate(state: AppState): Unit = {
    api.setCurrentProject(state.currentProject)

    js.timers.setTimeout(0) {
      LocalStorage.update("appState", state.toJSON)
    }
  }

  private
  def processAction(action: Action): Unit = action match {
    case LoadRepositories() =>
      val msg = l(
        action = LeonAction.loadRepositories,
        module = "main"
      )

      api.leonSocket.sendBuffered(JSON.stringify(msg))

    case LoadRepository(repo) =>
      val msg = l(
        action = LeonAction.loadRepository,
        module = "main",
        owner  = repo.owner,
        repo   = repo.name
      )

      api.leonSocket.sendBuffered(JSON.stringify(msg))

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

      api.leonSocket.sendBuffered(JSON.stringify(msg))

    case LoadFile(repo, file) =>
      val msg = l(
        action = LeonAction.loadFile,
        module = "main",
        owner  = repo.owner,
        repo   = repo.name,
        file   = file
      )

      api.leonSocket.sendBuffered(JSON.stringify(msg))

    case UpdateEditorCode(code) =>
      api.setEditorCode(code)

      Events.codeUpdated ! CodeUpdated()

    case SetCurrentProject(project) =>
      api.setCurrentProject(project)

      project.flatMap(_.code).foreach { code =>
        Actions.updateEditorCode ! UpdateEditorCode(code)
      }

    case SetTreatAsProject(value) =>
      api.setTreatAsProject(value)

    case DoGitOperation(op) =>
      api.getCurrentProject() match {
        case None =>
          console.error("No project is currently set, cannot perform Git operation")

        case Some(project) =>
          val commitMessage = op match {
            case GitOperation.Commit(msg) => msg
            case _ => ""
          }

          val msg = l(
            action  = LeonAction.doGitOperation,
            module  = "main",
            op      = op.name,
            msg     = commitMessage,
            project = l(
              owner  = project.owner,
              repo   = project.repo,
              branch = project.branch,
              file   = project.file
            )
          )

          api.leonSocket.send(JSON.stringify(msg))
      }

    case _ =>
  }

  private
  def render(state: AppState): Unit = {
    renderLogin(state)
    renderLoadRepoPanel(state)
  }

  private
  def renderLogin(state: AppState): Unit = {
    val el             = document.getElementById("login-modal")
    val showLoginModal = !state.isLoggedIn && state.showLoginModal

    def onRequestHide: Callback = Callback {
      Actions.toggleLoginModal ! ToggleLoginModal(false)
    }

    if (showLoginModal) {
      ReactDOM.render(LoginModal(onRequestHide), el)
    } else {
      ReactDOM.render(<.span(), el)
    }

    $("#login-btn").click { e: JQueryEventObject =>
      if (!shouldSkipLoginModal) {
        e.preventDefault()
        Actions.toggleLoginModal ! ToggleLoginModal(true)
      }
    }
  }

  private
  def shouldSkipLoginModal: Boolean =
    LocalStorage("hideLogin").map(_ == "true").getOrElse(false)

  private
  def renderLoadRepoPanel(state: AppState): Unit = {
    val panelEl = document.getElementById("load-repo-panel")

    if (panelEl =!= null) {
      ReactDOM.render(LoadRepositoryPanel(state), panelEl)
      $(panelEl).show()
    }
  }

}

