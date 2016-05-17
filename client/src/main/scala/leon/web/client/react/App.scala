/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react

import scala.concurrent.Future
import scala.scalajs.js
import js.Dynamic.{ literal => l, global => g }
import org.scalajs.dom.ext.LocalStorage
import org.scalajs.dom.{console, document}
import org.scalajs.jquery.{ jQuery => $, JQueryEventObject }
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import monifu.reactive.Observable
import monifu.concurrent.Implicits.globalScheduler
import leon.web.shared.messages._

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

  import leon.web.shared.{Action => LeonAction}

  lazy val isLoggedIn = g._leon_isLoggedIn.asInstanceOf[Boolean]

  def init(): Unit = {
    // Register the WebSocket handlers.
    api.registerMessageHandler(Handlers)

    val appState =
      LocalStorage("appState")
        .map((s: String) =>
          try {
            AppState.fromJSON(s)
          } catch {
            case e: Throwable =>
              println("Impossible to recover app state. Recreating a new one")
              AppState()
          }
        )
        .map(resetAppState _)
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

      api.sendBuffered(shared.messages.LoadRepositories)

      onEvent(Events.repositoriesLoaded) { e => state =>
        state.copy(repositories = Some(e.repos))
      }

      now(state)

    case LoadRepository(repo) =>
      api.sendBuffered(shared.messages.LoadRepository(owner = repo.owner, repo = repo.name))

      onEvent(Events.repositoryLoaded) { e => state =>
        state.copy(
          repository        = Some(e.repository),
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
      api.sendBuffered(shared.messages.SwitchBranch(owner = repo.owner, repo = repo.name, branch = branch))

      onEvent(Events.branchChanged) { e => state =>
        state.copy(
          branch = e.branch,
          files  = e.files match { case Some(f) => f case None => Array[String]() },
          file   = None
        )
      }

      now(state)

    case LoadFile(repo, file) =>
      api.sendBuffered(shared.messages.LoadFile(owner = repo.owner, repo = repo.name, file = file))

      onEvent(Events.fileLoaded) { e => state =>
        //println("Got file Load with content: " + e.content)
        api.setEditorCode(e.content)
        state.copy(file = Some((e.file, e.content)))
      }
      
      

      now(state)

    case ReloadCurrentFile() =>
      val msg =
        for {
          repo          <- state.repository
          (fileName, _) <- state.file
        }
        yield shared.messages.LoadFile(
          owner = repo.owner,
          repo   = repo.name,
          file   = fileName
        )

      msg foreach { msg =>
        api.sendBuffered(msg)

        onEvent(Events.fileLoaded) { e => state =>
          Actions dispatch UpdateEditorCode(e.content)
          state.copy(file = Some((e.file, e.content)))
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
          val msg = shared.messages.DoGitOperation(
            op      = op,
            project = project
          )

          api.sendMessage(msg)
      }

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
      Actions dispatch ToggleLoginModal(false)
    }

    if (showLoginModal) {
      ReactDOM.render(LoginModal(onRequestHide), el)
    } else {
      ReactDOM.render(<.span(), el)
    }

    $("#login-btn").click { e: JQueryEventObject =>
      if (!shouldSkipLoginModal) {
        e.preventDefault()
        Actions dispatch ToggleLoginModal(true)
      }
    }
  }

  private
  def shouldSkipLoginModal: Boolean =
    LocalStorage("hideLogin").map(_ === "true").getOrElse(false)

  private
  def renderLoadRepoPanel(state: AppState): Unit = {
    val panelEl = document.getElementById("load-repo-panel")

    if (panelEl =!= null) {
      ReactDOM.render(LoadRepositoryPanel(state), panelEl)
      $(panelEl).show()
    }
  }

}

