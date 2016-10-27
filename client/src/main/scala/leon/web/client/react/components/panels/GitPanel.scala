/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react
package components
package panels

import leon.web.client.react.utils._
import leon.web.client.react.components.modals._
import leon.web.shared.git._
import leon.web.shared.Repository
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

/** Toolbar for git commands such as
  * $ - Commit
  * $ - Pull
  * $ - Push
  * $ - etc.
  */
object GitPanel {

  case class Props(repo: Repository)

  case class State(showModalForOp: Option[String] = None)

  class Backend($: BackendScope[Props, State]) {

    case class GitButton(
      op: String,
      title: String,
      icon: String,
      remote: Boolean = false
    ) {
      val callback = showModal(op)
      val modal    = displayModal(op)
    }

    val buttons = Seq(
      GitButton(GitOperation.COMMIT, "Commit", "checklist"),
      GitButton(GitOperation.PUSH  , "Push",   "repo-push", true),
      GitButton(GitOperation.PULL  , "Pull",   "repo-pull", true),
      GitButton(GitOperation.RESET , "Reset",  "zap")
    )

    def showModal(op: String): Callback =
      $.modState(_.copy(showModalForOp = Some(op)))

    def doGitOperation(op: GitOperation): Callback =
      Actions dispatchCB DoGitOperation(op)

    def fetchStatus: Callback =
      doGitOperation(GitStatus)

    def displayModal(op: String) = op match {
      case GitOperation.COMMIT => CommitModal(onRequestHideModal)
      case GitOperation.PUSH   => PushModal(onRequestHideModal)
      case GitOperation.PULL   => PullModal(onRequestHideModal)
      case GitOperation.RESET  => ResetModal(onRequestHideModal)
    }

    def onRequestHideModal: Callback =
      $.modState(_.copy(showModalForOp = None))

    def renderButton(btn: GitButton) =
      <.li(
        <.button(
          ^.className := "btn btn-default",
          ^.onClick --> btn.callback,
          octicon(btn.icon, btn.title)
        )
    )

    def render(props: Props, state: State) =
      <.div(^.id := "git-ops-panel",
        state.showModalForOp.isDefined ?= displayModal(state.showModalForOp.get),
        <.ul(
          buttons
            .filter(props.repo.remote.isDefined || !_.remote)
            .map(renderButton)
        )
      )

  }

  val component =
    ReactComponentB[Props]("GitPanel")
      .initialState(State())
      .renderBackend[Backend]
      .build

  def apply(repo: Repository) = component(Props(repo))

}

