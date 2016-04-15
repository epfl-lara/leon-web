/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react

import japgolly.scalajs.react.Callback

import monifu.reactive.subjects._

import leon.web.client.HandlersTypes._
import leon.web.client.data.User
import leon.web.shared.{Project, Provider, GitOperation}

/** Actions that the React app can trigger.
 *  These will have side effects that are to be reflected
 *  in the global [[leon.web.client.react.AppState]]. */
sealed trait Action
case class LoadRepositories() extends Action
case class LoadRepository(repo: HRepository) extends Action
case class LoadFile(repo: HRepository, file: String) extends Action
case class ReloadCurrentFile() extends Action
case class SwitchBranch(repo: HRepository, branch: String) extends Action
case class UpdateEditorCode(code: String, updateEditor: Boolean = true) extends Action
case class ToggleLoadRepoModal(value: Boolean) extends Action
case class ToggleLoginModal(value: Boolean) extends Action
case class ToggleAccountModal(value: Boolean) extends Action
case class SetCurrentProject(project: Option[Project]) extends Action
case class SetTreatAsProject(value: Boolean) extends Action
case class DoGitOperation(op: GitOperation) extends Action
case class UnlinkAccount(provider: Provider) extends Action
case class UpdateUser(user: User) extends Action
case class UpdateState(state: AppState => AppState) extends Action

object Actions {

  val bus = PublishSubject[Action] // dump "Action"

  def dispatch(action: Action): Unit =
    bus onNext action

  def dispatchCB(action: Action): Callback = Callback {
    bus onNext action
  }

}

