/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react

import scala.Function.const

import monifu.reactive._
import monifu.reactive.subjects._

import monifu.concurrent.Implicits.globalScheduler

import leon.web.client.HandlersTypes._
import leon.web.client.syntax.Observer._
import leon.web.shared.Project

/** Actions that the React app can trigger.
 *  These will have side effects that are to be reflected
 *  in the global [[leon.web.client.react.AppState]]. */
sealed trait Action
case class LoadRepositories() extends Action
case class LoadRepository(repo: HRepository) extends Action
case class LoadFile(repo: HRepository, file: String) extends Action
case class SwitchBranch(repo: HRepository, branch: String) extends Action
case class UpdateEditorCode(code: String) extends Action
case class ToggleLoadRepoModal(value: Boolean) extends Action
case class ToggleLoginModal(value: Boolean) extends Action
case class SetCurrentProject(project: Option[Project]) extends Action
case class SetTreatAsProject(value: Boolean) extends Action

/**
  * Actions are how the React app performs side-effects.
  * To do so, a React component can send a message to one of
  * the [[monifu.react.subjects.Subject]] defined below.
  *
  * This message will be processed by the action handler specified
  * with [[leon.web.client.react.Actions.setActionHandler]] and will
  * typically push an event into one of the [[leon.web.client.react.Event]] bus.
  * This event will trigger a state transformation that, once applied, will
  * itself trigger a re-render of the whole components tree.
  */
object Actions {

  val loadRepositories    = PublishSubject[LoadRepositories]()     // dump "LoadRepositories"
  val loadRepository      = PublishSubject[LoadRepository]()       // dump "LoadRepository"
  val loadFile            = PublishSubject[LoadFile]()             // dump "LoadFile"
  val switchBranch        = PublishSubject[SwitchBranch]()         // dump "SwitchBranch"
  val updateEditorCode    = PublishSubject[UpdateEditorCode]()     // dump "UpdateEditorCode"
  val toggleLoadRepoModal = PublishSubject[ToggleLoadRepoModal]()  // dump "ToggleLoadRepoModal"
  val toggleLoginModal    = PublishSubject[ToggleLoginModal]()     // dump "ToggleLoginModal"
  val modState            = PublishSubject[AppState => AppState]() // dump "ModState"
  val setCurrentProject   = PublishSubject[SetCurrentProject]()    // dump "SetCurrentProject"
  val setTreatAsProject   = PublishSubject[SetTreatAsProject]()    // dump "SetTreatAsProject"

  lazy val currentProject = Observable.merge(
      setCurrentProject,
      Observable.combineLatest(
        Events.repositoryLoaded,
        Events.branchChanged,
        Events.fileLoaded
      )
      .map { case (r, b, f) =>
        val project = Project(
          owner  = r.repo.owner,
          repo   = r.repo.name,
          branch = b.branch,
          file   = f.fileName,
          code   = Some(f.content)
        )

        SetCurrentProject(Some(project))
      }
    )

  private
  var processAction: Action => Unit = x => {}

  /** Set the [[leon.web.client.actions.Action]] handler. */
  def setActionHandler(handler: Action => Unit): Unit = {
    processAction = handler
  }

  /** For each [[leon.web.client.react.Action]] [[monifu.reactive.subjects.Subject]]
    * defined above:
    *
    * $ 1. Process each action with the specified action handler.
    * $ 2. Listen to the corresponding [[leon.web.client.react.Event]].
    * $ 3. Map each of those events to a state-updating function.
    * $ 4. Push those functions into `updates`.
    *
    * @see [[leon.web.client.react.Event]]
    * @see [[leon.web.client.react.App]]
    * @see [[leon.web.client.react.AppState]]
    */
  def register(updates: Observer[AppState => AppState]) = {
    currentProject
      .doWork(processAction)
      .map { e =>
        e.project match {
          case None  => (state: AppState) =>
            state.copy(repository = None, files = Seq(), file = None, branches = Seq(), currentProject = None)

          case Some(project) => (state: AppState) =>
            state.copy(currentProject = Some(project))
        }
      }
      .subscribe(updates)

    setTreatAsProject
      .doWork(processAction)
      .map { e =>
        (state: AppState) => state.copy(treatAsProject = e.value)
      }
      .subscribe(updates)

    loadRepositories
      .doWork(processAction)
      .flatMap(_ => Events.repositoriesLoaded)
      .map { e =>
        (state: AppState) =>
          state.copy(repositories = Some(e.repos))
      }
      .subscribe(updates)

    loadRepository
      .doWork(processAction)
      .flatMap(_ => Events.repositoryLoaded)
      .doWork { e =>
        Events.branchChanged ! BranchChanged(e.repo.defaultBranch, e.files)
      }
      .map { e =>
        (state: AppState) =>
          state.copy(
            repository        = Some(e.repo),
            files             = e.files,
            branches          = e.branches,
            isLoadingRepo     = false,
            showLoadRepoModal = false
          )
      }
      .subscribe(updates)

    switchBranch
      .doWork(processAction)
      .flatMap(_ => Events.branchChanged)
      .map { e =>
        (state: AppState) =>
          state.copy(
            branch = Some(e.branch),
            files  = e.files
          )
      }
      .subscribe(updates)

    loadFile
      .doWork(processAction)
      .flatMap(_ => Events.fileLoaded)
      .map { e =>
        (state: AppState) =>
          state.copy(file = Some((e.fileName, e.content)))
      }
      .subscribe(updates)

    updateEditorCode
      .doWork(processAction)
      .flatMap(_ => Events.codeUpdated)
      .map(const(identity[AppState](_)))
      .subscribe(updates)

    toggleLoadRepoModal
      .doWork(processAction)
      .map { e =>
        (state: AppState) =>
          state.copy(
            showLoadRepoModal = e.value,
            isLoadingRepo     = false
          )
      }
      .subscribe(updates)

    toggleLoginModal
      .doWork(processAction)
      .map { e =>
        (state: AppState) =>
          state.copy(showLoginModal = e.value)
      }
      .subscribe(updates)

    modState
      .map { f =>
        (state: AppState) => f(state)
      }
      .subscribe(updates)
  }

}

