/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react
package components
package modals

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import leon.web.client.react._
import leon.web.client.react.attrs._

import leon.web.shared.messages.{DoGitOperation => _, _}
import leon.web.shared.git._

import monifu.concurrent.Implicits.globalScheduler

object CommitModal {

  case class Props(onRequestHide: Callback)

  case class Changeset(changes: Map[String, Set[String]], diff: String) {
    lazy val isEmpty: Boolean  = changes.values.forall(_.isEmpty)
    lazy val nonEmpty: Boolean = !isEmpty
  }

  case class State(
    changeset: Option[Changeset] = None,
    commitMessage: String = "",
    committing: Boolean = false
  )

  class Backend($: BackendScope[Props, State]) {

    def doGitOperation(op: GitOperation): Callback =
      Actions dispatchCB DoGitOperation(op)

    def fetchStatus: Callback =
      doGitOperation(GitStatus)

    def commit(message: String): Callback =
      listenForCommit >> doGitOperation(GitCommit(message))

    def onMount: Callback =
      listenForStatus >> fetchStatus

    def listenForStatus: Callback = Callback {
      Events.gitOperationDone
        .filter(_.op.name === GitOperation.STATUS)
        .take(1)
        .map(_.data)
        .doWork { res => onStatusUpdate(res).runNow() }
        .subscribe()
    }

    def onStatusUpdate(res: GitOperationResult): Callback = {
      res match {
        case GitStatusDiff(status, diff) =>
           val changes   = status.mapValues(_.toArray.toSet).toMap
          val changeset = Changeset(changes, diff)
    
          $.modState(_.copy(changeset = Some(changeset)))
        case _ => Callback { () }
      }
    }

    def listenForCommit: Callback = Callback {
      Events.gitOperationDone
        .filter(_.op.name === GitOperation.COMMIT)
        .take(1)
        .map(_.data)
        .doWork { _ => onCommitDone.runNow() }
        .subscribe()
    }

    def onCommitDone: Callback =
      $.modState(_.copy(committing = false)) >> onRequestHide

    def onRequestHide: Callback =
      $.props.flatMap(_.onRequestHide)

    def onCommitMessageChange(msg: String): Callback =
      $.modState(_.copy(commitMessage = msg))

    def onClickCommit: Callback =
      $.modState(_.copy(committing = true)) >>
      $.state.map(_.commitMessage).flatMap(commit)

    val cancelButton =
      <.button(
        ^.className := "btn",
        ^.onClick  --> onRequestHide,
        dataDismiss := "modal",
        "Cancel"
      )

    def commitButton(state: State) =
      <.a(
        ^.className := "btn btn-primary",
        ^.role      := "button",
        ^.onClick  --> onClickCommit,
        ^.disabled  := !enableCommitButton(state),
        if (state.committing) "Processing..." else "Commit"
      )

    def enableCommitButton(state: State): Boolean =
      state.changeset.exists(_.nonEmpty) &&
      state.commitMessage.nonEmpty

    def renderBody(state: State) = state.changeset match {
      case None                                  => <.div("Loading...")
      case Some(changeset) if changeset.isEmpty  => <.div("No changes.")
      case Some(changeset) if changeset.nonEmpty => <.div(
        git.StatusView(changeset.changes),
        git.CommitMessageView(state.commitMessage, onCommitMessageChange),
        git.DiffView(changeset.diff)
      )
    }

    def render(props: Props, state: State) =
      Modal(onRequestHide)(
        <.div(^.className := "modal-header",
          Modal.closeButton(onRequestHide),
          <.h3("Commit your changes")
        ),
        <.div(^.className := "modal-body",
          renderBody(state)
        ),
        renderFooter(props, state)
      )

    def renderFooter(props: Props, state: State) = {
      <.div(^.className := "modal-footer",
        cancelButton,
        commitButton(state)
      )
    }

  }

  val component =
    ReactComponentB[Props]("CommitModal")
      .initialState(State())
      .renderBackend[Backend]
      .componentDidMount(_.backend.onMount)
      .build

  def apply(onRequestHide: Callback) =
    component(Props(onRequestHide))

}

