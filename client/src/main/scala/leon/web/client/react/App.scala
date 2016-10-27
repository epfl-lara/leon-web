/* Copyright 2009-2016 EPFL, Lausanne */

package leon.web
package client
package react

import scala.concurrent.Future
import scala.scalajs.js
//import js.Dynamic.{ literal => l, global => g }
import org.scalajs.dom.ext.LocalStorage
import org.scalajs.dom.{/*console, */document}
import org.scalajs.jquery.{ jQuery => $, JQueryEventObject }
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import monifu.reactive.Observable
import monifu.concurrent.Implicits.globalScheduler
import leon.web.shared.messages._
import leon.web.client.react.components.modals._
import leon.web.client.react.components.panels._
import leon.web.client.data.UserManager

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

  lazy val initialUser = UserManager.initial

  def init(): Unit = {
    // Register the WebSocket handlers.
    api.registerMessageHandler(Handlers)

    val appState =
      LocalStorage("appState")
        .map((s: String) =>
          try {
            val res = AppState.fromJSON(s)
            if(!res.treatAsProject) {
              api.setTreatAsProject(res.treatAsProject)
            }
            res
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
    if (initialUser.isDefined) {
      restoreAppState(appState.initial)
    }

    injectEvents()
    bindToolbarButtons()
  }

  private
  def injectEvents(): Unit = {
    Events.userUpdated.foreach { case UserUpdated(user) =>
      Actions dispatch UpdateUser(user)
    }
  }

  private
  def resetAppState(state: AppState): AppState = {
    val isLoggedIn = initialUser.isDefined

    state.copy(
      user             = initialUser,
      isLoggedIn       = isLoggedIn,
      showLoginModal   = state.showLoginModal && !isLoggedIn,
      showAccountModal = isLoggedIn && state.showAccountModal,
      repository       = if (isLoggedIn) state.repository else None,
      branch           = if (isLoggedIn) state.branch     else None,
      file             = if (isLoggedIn) state.file       else None,
      isLoadingRepo    = false
    )
  }

  private
  def restoreAppState(state: AppState): Unit = {
    println("Restoring application state...")

    api.setRepositoryState(state.repoState)
  }

  private
  def onStateUpdate(state: AppState): Unit = {
    api.setRepositoryState(state.repoState)

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
      Backend.repository.loadRepositories()

      onEvent(Events.repositoriesLoaded) { e => state =>
        state.copy(repositories = Some(e.repos))
      }

      now(state)

    case LoadRepository(repo) =>
      api.sendBuffered(shared.messages.LoadRepository(repo.desc))

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
      api.sendBuffered(shared.messages.SwitchBranch(repo.desc, branch))

      onEvent(Events.branchChanged) { e => state =>
        state.copy(
          branch = e.branch,
          files  = e.files match { case Some(f) => f case None => Array[String]() },
          file   = None
        )
      }

      now(state)

    case LoadFile(repo, file) =>
      api.sendBuffered(shared.messages.LoadFile(repo = repo.desc, file = file))

      onEvent(Events.fileLoaded) { e => state =>
        //println("Got file Load with content: " + e.content)
        val newState = state.copy(file = Some((e.file, e.content)))
        js.timers.setTimeout(0) { 
          api.setEditorCode(e.content)
        }
        newState
      }
      
      

      now(state)

    case ReloadCurrentFile() =>
      val infos =
        for {
          repo      <- state.repository
          (file, _) <- state.file
        }
        yield shared.messages.LoadFile(
          repo   = repo.desc,
          file   = file
        )

      infos foreach { msg =>
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

    case SetRepositoryState(repoState) =>
      api.setRepositoryState(repoState)

      repoState.flatMap(_.code).foreach { code =>
        Actions dispatch UpdateEditorCode(code)
      }

      val newState = repoState match {
        case None    => state.unloadRepo
        case Some(_) => state
      }

      now(newState)

    case SetTreatAsProject(value) =>
      api.setTreatAsProject(value)

      now {
        state.copy(treatAsProject = value)
      }

    case DoGitOperation(op) =>
      api.getRepositoryState match {
        case None =>
          throw new Exception("No repository is currently loaded, cannot perform Git operation")

        case Some(repoState) =>
          val msg = shared.messages.DoGitOperation(
            op        = op,
            repoState = repoState
          )

          api.sendMessage(msg)
          now(state)
      }

    case UpdateUser(user) => now {
      state.copy(user = Some(user))
    }

    case UnlinkAccount(provider) =>
      Backend.main.unlinkAccount(provider)

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
      if (show) LoginModal(state.user, onRequestHide)
      else      <.span()

    ReactDOM.render(component, el)
  }

  private def renderAccount(state: AppState): Unit = {
    val el   = document.getElementById("account-modal")
    val show = state.isLoggedIn &&
               state.showAccountModal &&
               state.user.isDefined

    def onRequestHide: Callback = Callback {
      Actions dispatch ToggleAccountModal(false)
    }

    val component: ReactElement =
      if (show) AccountModal(state.user.get, onRequestHide)
      else      <.span()

    ReactDOM.render(component, el)
  }

  private
  def renderLoadRepoPanel(state: AppState): Unit = {
    val el   = document.getElementById("load-repo-panel")
    val show = state.user.flatMap(_.github).isDefined

    val component: ReactElement =
      if (show) LoadRepositoryPanel(state)
      else     <.span()

    ReactDOM.render(component, el)
  }

}

