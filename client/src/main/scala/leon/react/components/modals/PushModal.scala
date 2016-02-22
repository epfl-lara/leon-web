/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react
package components
package modals

import scala.scalajs.js

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import leon.web.client.react._
import leon.web.client.react.attrs._
import leon.web.client.utils.GitHubURL
import leon.web.client.syntax.observer._
import leon.web.client.HandlersTypes.{HGitOperationResult, HCommit}

import leon.web.shared.GitOperation

import monifu.concurrent.Implicits.globalScheduler

object PushModal {

  val COMMITS_COUNT = 10

  case class Props(onRequestHide: Callback)

  case class State(pushing: Boolean = false, commits: Option[Seq[HCommit]] = None)

  class Backend($: BackendScope[Props, State]) {

    def doGitOperation(op: GitOperation): Callback = Callback {
      Actions.doGitOperation ! DoGitOperation(op)
    }

    def push: Callback =
      listenForPush >> doGitOperation(GitOperation.Push)

    def onMount: Callback =
      listenForLog >> doGitOperation(GitOperation.Log(COMMITS_COUNT))

    def listenForLog: Callback = Callback {
      Events.gitOperationDone
        .filter(_.result.op === GitOperation.LOG)
        .take(1)
        .map(_.result)
        .doWork { res => onLogUpdate(res).runNow() }
        .subscribe()
    }

    def onLogUpdate(res: HGitOperationResult): Callback = {
      val commits = res.data.asInstanceOf[js.Array[HCommit]]
      $.modState(_.copy(commits = Some(commits.toSeq)))
    }

    def listenForPush: Callback = Callback {
      Events.gitOperationDone
        .filter(_.result.op === GitOperation.PUSH)
        .take(1)
        .map(_.result)
        .doWork { _ => onPushDone.runNow() }
        .subscribe()
    }

    def onPushDone: Callback =
      $.modState(_.copy(pushing = false)) >> onRequestHide

    def onRequestHide: Callback =
      $.props.flatMap(_.onRequestHide)

    def onClickPush: Callback =
      $.modState(_.copy(pushing = true)) >> push

    val cancelButton =
      <.button(
        ^.className := "btn",
        ^.onClick  --> onRequestHide,
        dataDismiss := "modal",
        "Cancel"
      )

    def pushButton(state: State) =
      <.a(
        ^.className := "btn btn-primary",
        ^.role      := "button",
        ^.onClick  --> onClickPush,
        if (state.pushing) "Pushing..." else "Push"
      )

    def renderBody(state: State) = state.commits match {
      case None          => <.div("Loading...")
      case Some(Nil)     => <.div("No commits")
      case Some(commits) => <.div(^.className := "git-log-view",
        <.h5(s"Last ${COMMITS_COUNT} commits:"),
        <.ul(^.className := "commits",
          commits.map(c => <.li(renderCommit(c)))
        )
      )
    }

    def renderCommit(c: HCommit) =
      <.span(^.className := "commit",
        <.span(^.className := "hash", c.hash.substring(0, 8)),
        " - ",
        <.span(^.className := "msg", c.shortMessage)
      )

    def render(props: Props, state: State) =
      Modal(onRequestHide)(
        <.div(^.className := "modal-header",
          Modal.closeButton(onRequestHide),
          <.h3("Push your changes")
        ),
        <.div(^.className := "modal-body",
          renderBody(state)
        ),
        renderFooter(props, state)
      )

    def renderFooter(props: Props, state: State) = {
      <.div(^.className := "modal-footer",
        cancelButton,
        pushButton(state)
      )
    }

  }

  val component =
    ReactComponentB[Props]("PushModal")
      .initialState(State())
      .renderBackend[Backend]
      .componentDidMount(_.backend.onMount)
      .build

  def apply(onRequestHide: Callback) =
    component(Props(onRequestHide))

}

