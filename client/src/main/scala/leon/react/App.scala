/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react

import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.scalajs.js.Dynamic.{ literal => l, global => g }

import org.scalajs.dom.document
import org.scalajs.dom.ext.LocalStorage

import org.scalajs.jquery
import org.scalajs.jquery.{ jQuery => $, JQueryEventObject }

import japgolly.scalajs.react._

import monifu.concurrent.Implicits.globalScheduler

import leon.web.client.syntax.Observer._
import leon.web.client.syntax.BufferedWebSocketOps._

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
        .map(_.copy(isLoggedIn = isLoggedIn, isLoadingRepo = false))
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
      api.setCurrentProject(appState.initial.currentProject)
      Actions.setTreatAsProject ! SetTreatAsProject(appState.initial.treatAsProject)
    }
  }

  private
  def onStateUpdate(state: AppState): Unit = {
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

