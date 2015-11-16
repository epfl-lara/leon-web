/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web.client
package react
package components
package modals

import scala.concurrent.duration._

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import leon.web.client.react._
import leon.web.client.react.attrs._
import leon.web.client.HandlersTypes.HRepository

import monifu.concurrent.Implicits.globalScheduler

/** Allows users to pick a repository in a list, and clone/pull it.
  * Also displays the current state of the clone/pull operation.
  */
object LoadRepositoryModal {

  case class Props(
    onSelect: HRepository => Callback,
    isOpen: Boolean = false,
    cloning: Boolean = false,
    repos: Option[Seq[HRepository]] = None
  )

  case class State(
    selectedRepo: Option[HRepository] = None,
    cloneProgress: Option[GitProgress] = None
  )

  class Backend($: BackendScope[Props, State]) {

    def onSelectRepo(repo: HRepository): Callback = {
      $.modState(_.copy(
        selectedRepo = Some(repo),
        cloneProgress = None
      ))
    }

    def subscribeToProgress: Callback = Callback {
      // We listen for this event here instead of
      // relying on the global app state for performance reasons.
      Events.gitProgress.throttleLast(500 millis).doWork(p => {
        $.modState(_.copy(cloneProgress = Some(p))).runNow()
      }).subscribe()
    }

    def onClickLoad(e: ReactMouseEvent): Callback =
      e.preventDefaultCB >>
      subscribeToProgress >>
      $.props.zip($.state) flatMap { case (props, state) =>
        state.selectedRepo
          .map(props.onSelect(_))
          .getOrElse(Callback.empty)
      }

    val cancelButton =
      <.button(
        ^.className := "btn",
        dataDismiss := "modal",
        "Cancel"
      )

    def loadButton(cloning: Boolean) =
      <.a(
        ^.className := "btn btn-primary",
        ^.role      := "button",
        ^.onClick  ==> onClickLoad,
        ^.disabled  := cloning,
        if (cloning) "Loading..." else "Load"
      )

    val loading = Spinner()

    def render(props: Props, state: State) =
      Modal(props.isOpen)(
        <.div(^.className := "modal-header",
          Modal.closeButton,
          <.h3("Load a repository from GitHub")
        ),
        <.div(^.className := "modal-body",
          <.p(
            """Pick a repository to load from the list below:"""
          ),
          props.repos match {
            case None        => loading
            case Some(repos) =>
              RepositoryList(
                repos = repos,
                onSelect = onSelectRepo,
                disabled = props.cloning
              )
          }
        ),
        renderFooter(props, state)
      )

    def renderFooter(props: Props, state: State) = {
      def onlyIf(cond: Boolean)(el: => TagMod): TagMod =
        if (cond) el else EmptyTag

      <.div(^.className := "modal-footer",
        onlyIf(props.cloning && state.cloneProgress.isDefined) {
          renderCloneProgress(state.cloneProgress.get)
        },
        onlyIf(!props.cloning) {
          cancelButton
        },
        onlyIf(state.selectedRepo.isDefined) {
          loadButton(props.cloning)
        }
      )
    }

    def renderCloneProgress(cp: GitProgress) =
      <.span(^.className := "clone-progress",
        s"${cp.task}",
        cp.percentage.map(p => <.span(s" ($p%)")).getOrElse(EmptyTag)
      )
  }

  val component =
    ReactComponentB[Props]("LoadRepositoryModal")
      .initialState(State())
      .renderBackend[Backend]
      .build

  def apply(
    onSelect: HRepository => Callback,
    isOpen: Boolean = false,
    loading: Boolean = false,
    repos: Option[Seq[HRepository]] = None
  ) = component(Props(onSelect, isOpen, loading, repos))

}

