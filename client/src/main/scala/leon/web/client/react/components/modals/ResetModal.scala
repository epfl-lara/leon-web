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

import leon.web.shared.GitOperation

import monifu.concurrent.Implicits.globalScheduler

object ResetModal {

  case class Props(onRequestHide: Callback)

  case class State(resetting: Boolean = false)

  class Backend($: BackendScope[Props, State]) {

    def doGitOperation(op: GitOperation): Callback =
      Actions dispatchCB DoGitOperation(op)

    def reset: Callback =
      listenForReset >> doGitOperation(GitOperation.Reset)

    def listenForReset: Callback = Callback {
      Events.gitOperationDone
        .filter(_.result.op === GitOperation.RESET)
        .take(1)
        .map(_.result)
        .doWork { _ => onResetDone.runNow() }
        .subscribe()
    }

    def onResetDone: Callback =
      reloadCurrentFile >>
      $.modState(_.copy(resetting = false)) >>
      onRequestHide

    def reloadCurrentFile: Callback =
      Actions dispatchCB ReloadCurrentFile()

    def onRequestHide: Callback =
      $.props.flatMap(_.onRequestHide)

    def onClickReset: Callback =
      $.modState(_.copy(resetting = true)) >> reset

    val cancelButton =
      <.button(
        ^.className := "btn",
        ^.onClick  --> onRequestHide,
        dataDismiss := "modal",
        "Cancel"
      )

    def resetButton(state: State) =
      <.a(
        ^.className := "btn btn-primary",
        ^.role      := "button",
        ^.onClick  --> onClickReset,
        if (state.resetting) "Resetting..." else "Reset"
      )

    def render(props: Props, state: State) =
      Modal(onRequestHide)(
        <.div(^.className := "modal-header",
          Modal.closeButton(onRequestHide),
          <.h3("Are you sure you want to reset your changes?")
        ),
        <.div(^.className := "modal-body",
          <.p("""This operation will get rid of any modifications made to any of the files in the repository,
                |and cannot be undone (it is equivalent to 'git reset --hard HEAD').
                |Proceed with caution.""".stripMargin)
        ),
        renderFooter(props, state)
      )

    def renderFooter(props: Props, state: State) = {
      <.div(^.className := "modal-footer",
        cancelButton,
        resetButton(state)
      )
    }

  }

  val component =
    ReactComponentB[Props]("ResetModal")
      .initialState(State())
      .renderBackend[Backend]
      .build

  def apply(onRequestHide: Callback) =
    component(Props(onRequestHide))

}

