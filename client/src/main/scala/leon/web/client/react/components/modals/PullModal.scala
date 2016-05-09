/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react
package components
package modals

import scala.concurrent.duration._

import scala.scalajs.js

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import leon.web.client.react._
import leon.web.client.react.attrs._
import leon.web.client.utils.GitHubURL

import leon.web.shared.GitOperation

import monifu.concurrent.Implicits.globalScheduler

object PullModal {

  val COMMITS_COUNT = 10

  case class Props(onRequestHide: Callback)

  case class State(pulling: Boolean = false, progress: Option[GitProgress] = None)

  class Backend($: BackendScope[Props, State]) {

    def doGitOperation(op: GitOperation): Callback =
      Actions dispatchCB DoGitOperation(op)

    def pull: Callback =
      subscribeToPullUpdate >>
      subscribeToPullDone >>
      doGitOperation(GitOperation.Pull)

    def subscribeToPullUpdate: Callback = Callback {
      Events.gitProgress
        .throttleLast(500.millis)
        .doWork { p =>
          onPullUpdate(p).runNow()
        }
        .subscribe()
    }

    def subscribeToPullDone: Callback = Callback {
      Events.gitOperationDone
        .map(_.result)
        .filter(_.op === GitOperation.PULL)
        .take(1)
        .doWork { _ => onPullDone.runNow() }
        .subscribe()
    }

    def onPullUpdate(progress: GitProgress): Callback =
      $.modState(_.copy(progress = Some(progress)))

    def onPullDone: Callback =
      reloadFile >>
      $.modState(_.copy(pulling = false)) >>
      onRequestHide

    def reloadFile: Callback =
      Actions dispatchCB ReloadCurrentFile()

    def onRequestHide: Callback =
      $.props.flatMap(_.onRequestHide)

    def onClickPull: Callback =
      $.modState(_.copy(pulling = true)) >> pull

    val cancelButton =
      <.button(
        ^.className := "btn",
        ^.onClick  --> onRequestHide,
        dataDismiss := "modal",
        "Cancel"
      )

    def pullButton(state: State) =
      <.a(
        ^.className := "btn btn-primary",
        ^.role      := "button",
        ^.onClick  --> onClickPull,
        if (state.pulling) "Pulling..." else "Pull"
      )

    def renderBody(state: State) = state.progress match {
      case None     => <.div("Incorporate changes from the remote repository into the current branch.")
      case Some(pp) => <.div(^.className := "git-pull-view",
        <.span(^.className := "pull-progress",
          s"${pp.task}",
          pp.percentage.map(p => <.span(s" ($p%)")).getOrElse(EmptyTag)
        )
      )
    }

    def render(props: Props, state: State) =
      Modal(onRequestHide)(
        <.div(^.className := "modal-header",
          Modal.closeButton(onRequestHide),
          <.h3("Pull remote repository")
        ),
        <.div(^.className := "modal-body",
          renderBody(state)
        ),
        renderFooter(props, state)
      )

    def renderFooter(props: Props, state: State) = {
      <.div(^.className := "modal-footer",
        cancelButton,
        pullButton(state)
      )
    }

  }

  val component =
    ReactComponentB[Props]("PullModal")
      .initialState(State())
      .renderBackend[Backend]
      .build

  def apply(onRequestHide: Callback) =
    component(Props(onRequestHide))

}

