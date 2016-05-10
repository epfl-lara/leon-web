/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react

import japgolly.scalajs.react.Callback
import monifu.reactive.subjects._
import leon.web.shared.messages._
import leon.web.shared.github._
import leon.web.shared._

trait Action

case class LoadRepositories() extends Action
case class ReloadCurrentFile() extends Action
case class UpdateState(state: AppState => AppState) extends Action
case class UpdateEditorCode(code: String, updateEditor: Boolean = true) extends Action
case class SwitchBranch(repo: Repository, branch: String) extends Action
case class LoadRepository(repo: Repository) extends Action
case class LoadFile(repo: Repository, file: String) extends Action
case class DoGitOperation(op: GitOperation) extends Action
case class ToggleLoadRepoModal(value: Boolean) extends Action
case class ToggleLoginModal(value: Boolean) extends Action
case class SetCurrentProject(project: Option[Project]) extends Action
case class SetTreatAsProject(value: Boolean) extends Action

object Actions {

  val bus = PublishSubject[Action] // dump "Action"

  def dispatch(action: Action): Unit =
    bus onNext action

  def dispatchCB(action: Action): Callback = Callback {
    bus onNext action
  }

}

