package leon.web.client
package actions

import monifu.reactive._
import monifu.reactive.subjects._

import monifu.concurrent.Implicits.globalScheduler

import leon.web.client.react.AppState
import leon.web.client.events.Events
import leon.web.client.HandlersTypes._
import leon.web.client.syntax.Observer._

sealed trait Action
case class LoadRepositories() extends Action
case class LoadFiles(repo: HRepository) extends Action
case class LoadFile(repo: HRepository, file: String) extends Action
case class UpdateEditorCode(code: String) extends Action
case class ToggleLoadRepoModal(value: Boolean) extends Action

object Actions {

  val loadRepositories    = PublishSubject[LoadRepositories]()
  val loadFiles           = PublishSubject[LoadFiles]()
  val loadFile            = PublishSubject[LoadFile]()
  val updateEditorCode    = PublishSubject[UpdateEditorCode]()
  val toggleLoadRepoModal = PublishSubject[ToggleLoadRepoModal]()
  val modState            = PublishSubject[AppState => AppState]

  private
  var processAction: Action => Unit = x => {}

  def setActionHandler(handler: Action => Unit): Unit = {
    processAction = handler
  }

  def register(updates: Observer[AppState => AppState]) = {
    modState
      .map { f =>
        (state: AppState) => f(state)
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

    loadFiles
      .doWork(processAction)
      .flatMap(_ => Events.filesLoaded)
      .map { e =>
        (state: AppState) =>
          state.copy(files = e.files,
                     isLoadingRepo = false,
                     showLoadRepoModal = false)
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

      updates ! identity
  }

}

