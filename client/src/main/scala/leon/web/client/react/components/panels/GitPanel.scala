/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react
package components
package panels

import leon.web.client.react.utils._
import leon.web.client.react.components.modals._
import leon.web.shared.git._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

/** Toolbar for git commands such as
  * $ - Commit
  * $ - Pull
  * $ - Push
  * $ - etc.
  */
object GitPanel {

  case class State(showModalForOp: Option[String] = None)

  class Backend($: BackendScope[Unit, State]) {

    def onCommit = showModal(GitOperation.COMMIT)
    def onPush   = showModal(GitOperation.PUSH)
    def onPull   = showModal(GitOperation.PULL)
    def onReset  = showModal(GitOperation.RESET)

    def showModal(op: String): Callback =
      $.modState(_.copy(showModalForOp = Some(op)))

    def doGitOperation(op: GitOperation): Callback =
      Actions dispatchCB DoGitOperation(op)

    def fetchStatus: Callback =
      doGitOperation(GitStatus)

    def displayModal(op: String) =
      op match {
        case GitOperation.COMMIT => CommitModal(onRequestHideModal)
        case GitOperation.PUSH   => PushModal(onRequestHideModal)
        case GitOperation.PULL   => PullModal(onRequestHideModal)
        case GitOperation.RESET  => ResetModal(onRequestHideModal)
      }

    def onRequestHideModal: Callback =
      $.modState(_.copy(showModalForOp = None))

    def render(state: State) =
      <.div(^.id := "git-ops-panel",
        state.showModalForOp.isDefined ?= displayModal(state.showModalForOp.get),
        <.ul(
          <.li(
            <.button(^.className := "btn btn-default",
              ^.onClick --> onCommit,
              octicon("checklist", "Commit")
            )
          ),
          <.li(
            <.button(^.className := "btn btn-default",
              ^.onClick --> onPush,
              octicon("repo-push", "Push")
            )
          ),
          <.li(
            <.button(^.className := "btn btn-default",
              ^.onClick --> onPull,
              octicon("repo-pull", "Pull")
            )
          ),
          <.li(
            <.button(^.className := "btn btn-default",
              ^.onClick --> onReset,
              octicon("zap", "Reset")
            )
          )
        )
      )

  }

  val component =
    ReactComponentB[Unit]("GitPanel")
      .initialState(State())
      .renderBackend[Backend]
      .build

  def apply() = component()

}

