/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web.client
package react

import monifu.reactive._
import monifu.reactive.subjects._

import monifu.concurrent.Implicits.globalScheduler

import leon.web.client.HandlersTypes._
import leon.web.client.syntax.Observer._

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

  val loadRepositories    = PublishSubject[LoadRepositories]()
  val loadRepository      = PublishSubject[LoadRepository]()
  val loadFile            = PublishSubject[LoadFile]()
  val switchBranch        = PublishSubject[SwitchBranch]()
  val updateEditorCode    = PublishSubject[UpdateEditorCode]()
  val toggleLoadRepoModal = PublishSubject[ToggleLoadRepoModal]()
  val modState            = PublishSubject[AppState => AppState]

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
      .map { e =>
        (state: AppState) =>
          state.copy(
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
      .doWork { e =>
        updateEditorCode ! UpdateEditorCode(e.content)
      }
      .map { e =>
        (state: AppState) =>
          state.copy(file = Some((e.fileName, e.content)))
      }
      .subscribe(updates)

    updateEditorCode
      .doWork(processAction)
      .flatMap(_ => Events.codeUpdated)
      .map(_ => identity[AppState] _)
      .subscribe(updates)

    toggleLoadRepoModal
      .doWork(processAction)
      .map { e =>
        (state: AppState) =>
          state.copy(showLoadRepoModal = e.value)
      }
      .subscribe(updates)

    modState
      .map { f =>
        (state: AppState) => f(state)
      }
      .subscribe(updates)
  }

}

