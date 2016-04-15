/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react

import scala.concurrent.Future

import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.scalajs.js.Dynamic.{ literal => l, global => g }

import org.scalajs.dom.ext.LocalStorage
import org.scalajs.dom.{console, document}

import org.scalajs.jquery
import org.scalajs.jquery.{ jQuery => $, JQueryEventObject }

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import monifu.reactive.Observable
import monifu.concurrent.Implicits.globalScheduler

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

  import leon.web.client.react.components.modals._
  import leon.web.client.react.components.panels._

  import leon.web.data.User
  import leon.web.shared.{Action => LeonAction}

  lazy val isLoggedIn = g._leon_isLoggedIn.asInstanceOf[Boolean]

  lazy val user = User.current

  def init(): Unit = {
    // Register the WebSocket handlers.
    Handlers.register(api.handlers)

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
    Actions.bus.map(processAction).subscribe(appState.updates)

    // If the user is logged-in and was working on a project,
    // restore such project.
    if (isLoggedIn) {
      restoreAppState(appState.initial)
    }

    bindToolbarButtons()
  }

  private
  def resetAppState(state: AppState): AppState = state.copy(
    isLoggedIn       = isLoggedIn,
    showLoginModal   = state.showLoginModal && !isLoggedIn,
    showAccountModal = isLoggedIn && state.showAccountModal,
    repository       = if (isLoggedIn) state.repository else None,
    branch           = if (isLoggedIn) state.branch     else None,
    file             = if (isLoggedIn) state.file       else None,
    isLoadingRepo    = false
  )

  private
  def restoreAppState(state: AppState): Unit = {
    println("Restoring application state...")

    api.setCurrentProject(state.currentProject)
  }

  private
  def onStateUpdate(state: AppState): Unit = {
    api.setCurrentProject(state.currentProject)

    js.timers.setTimeout(0) {
      LocalStorage.update("appState", state.toJSON)
    }
  }

  private
  def now[A](x: A): Future[A] =
    Future.successful(x)

  private
  def onEvent[E <: Event](event: Observable[E])(f: E => AppState => AppState): Unit =
    event
      .head
      .doWork { e =>
        Actions dispatch UpdateState(f(e))
      }
      .subscribe()

  private
  def processAction(action: Action)(state: AppState): Future[AppState] = action match {
    case UpdateState(update) =>
      now {
        update(state)
      }

    case LoadRepositories() =>
      val msg = l(
        action = LeonAction.loadRepositories,
        module = "main"
      )

      api.leonSocket.sendBuffered(JSON.stringify(msg))

      onEvent(Events.repositoriesLoaded) { e => state =>
        state.copy(repositories = Some(e.repos))
      }

      now(state)

    case LoadRepository(repo) =>
      val msg = l(
        action = LeonAction.loadRepository,
        module = "main",
        owner  = repo.owner,
        repo   = repo.name
      )

      api.leonSocket.sendBuffered(JSON.stringify(msg))

      onEvent(Events.repositoryLoaded) { e => state =>
        state.copy(
          repository        = Some(e.repo),
          files             = e.files,
          file              = None,
          branches          = e.branches,
          branch            = Some(e.currentBranch),
          isLoadingRepo     = false,
          showLoadRepoModal = false
        )
      }

      now {
        state.copy(
          repository    = Some(repo),
          branch        = Some(repo.defaultBranch),
          isLoadingRepo = true
        )
      }

    case SwitchBranch(repo, branch) =>
      val msg = l(
        action = LeonAction.switchBranch,
        module = "main",
        owner  = repo.owner,
        repo   = repo.name,
        branch = branch
      )

      api.leonSocket.sendBuffered(JSON.stringify(msg))

      onEvent(Events.branchChanged) { e => state =>
        state.copy(
          branch = Some(e.branch),
          files  = e.files,
          file   = None
        )
      }

      now(state)

    case LoadFile(repo, file) =>
      val msg = l(
        action = LeonAction.loadFile,
        module = "main",
        owner  = repo.owner,
        repo   = repo.name,
        file   = file
      )

      api.leonSocket.sendBuffered(JSON.stringify(msg))

      onEvent(Events.fileLoaded) { e => state =>
        state.copy(file = Some((e.fileName, e.content)))
      }

      now(state)

    case ReloadCurrentFile() =>
      val msg =
        for {
          repo          <- state.repository
          (fileName, _) <- state.file
        }
        yield l(
          action = LeonAction.loadFile,
          module = "main",
          owner  = repo.owner,
          repo   = repo.name,
          file   = fileName
        )

      msg foreach { msg =>
        api.leonSocket.sendBuffered(JSON.stringify(msg))

        onEvent(Events.fileLoaded) { e => state =>
          Actions dispatch UpdateEditorCode(e.content)
          state.copy(file = Some((e.fileName, e.content)))
        }
      }

      now(state)

    case UpdateEditorCode(code, updateEditor) =>
      if (updateEditor)
        api.setEditorCode(code)

      now {
        val file = state.file.map { case (name, _) =>
          (name, code)
        }

        state.copy(file = file)
      }

    case SetCurrentProject(project) =>
      api.setCurrentProject(project)

      project.flatMap(_.code).foreach { code =>
        Actions dispatch UpdateEditorCode(code)
      }

      val newState = project match {
        case None    => state.unloadProject
        case Some(_) => state
      }

      now(newState)

    case SetTreatAsProject(value) =>
      api.setTreatAsProject(value)

      now {
        state.copy(treatAsProject = value)
      }

    case DoGitOperation(op) =>
      api.getCurrentProject() match {
        case None =>
          console.error("No project is currently set, cannot perform Git operation")

        case Some(project) =>
          val data = op match {
            case GitOperation.Commit(msg) => l(msg = msg)
            case GitOperation.Push(force) => l(force = force)
            case GitOperation.Log(count)  => l(count = count)
            case _                        => l()
          }

          val msg = l(
            action  = LeonAction.doGitOperation,
            module  = "main",
            op      = op.name,
            data    = data,
            project = l(
              owner  = project.owner,
              repo   = project.repo,
              branch = project.branch,
              file   = project.file
            )
          )

          api.leonSocket.send(JSON.stringify(msg))
      }

      now(state)

    case UnlinkAccount(provider) =>
      val msg = l(
        action = LeonAction.unlinkAccount,
        module = "main",
        provider = provider.id
      )

      api.leonSocket.send(JSON.stringify(msg))

      now(state)

    case ToggleLoadRepoModal(value) =>
      now {
        state.copy(
          showLoadRepoModal = value,
          isLoadingRepo     = false
        )
      }

    case ToggleLoginModal(value) =>
      now {
        state.copy(showLoginModal = value)
      }

    case ToggleAccountModal(value) =>
      now {
        state.copy(showAccountModal = value)
      }
  }

  private
  def render(state: AppState): Unit = {
    renderLogin(state)
    renderAccount(state)
    renderLoadRepoPanel(state)
  }

  private
  def bindToolbarButtons(): Unit = {
    $("#login-btn").click { e: JQueryEventObject =>
      e.preventDefault()
      Actions dispatch ToggleLoginModal(true)
    }

    $("#account-btn").click { e: JQueryEventObject =>
      e.preventDefault()
      Actions dispatch ToggleAccountModal(true)
    }
  }

  private
  def renderLogin(state: AppState): Unit = {
    val el   = document.getElementById("login-modal")
    val show = !state.isLoggedIn && state.showLoginModal

    def onRequestHide: Callback = Callback {
      Actions dispatch ToggleLoginModal(false)
    }

    val component: ReactElement =
      if (show) LoginModal(user, onRequestHide)
      else      <.span()

    ReactDOM.render(component, el)
  }

  private def renderAccount(state: AppState): Unit = {
    val el   = document.getElementById("account-modal")
    val show = state.isLoggedIn &&
               state.showAccountModal &&
               user.isDefined

    def onRequestHide: Callback = Callback {
      Actions dispatch ToggleAccountModal(false)
    }

    val component: ReactElement =
      if (show) AccountModal(user.get, onRequestHide)
      else      <.span()

    ReactDOM.render(component, el)
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

