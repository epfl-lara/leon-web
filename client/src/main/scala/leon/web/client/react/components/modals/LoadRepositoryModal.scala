/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react
package components
package modals

import scala.concurrent.duration._

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import leon.web.client.react._
import leon.web.client.react.attrs._
import leon.web.shared.messages._
import shared.Repository
import leon.web.shared.Provider

import monifu.concurrent.Implicits.globalScheduler

/** Allows users to pick a repository in a list, and clone/pull it.
  * Also displays the current state of the clone/pull operation.
  */
object LoadRepositoryModal {

  case class Props(
    onSelect: Repository => Callback,
    cloning: Boolean = false,
    repos: Option[Map[Provider, Seq[Repository]]] = None
  ) {
    val providers: Seq[Provider] = repos.map(_.keys.toSeq).getOrElse(Seq())
  }

  case class State(
    selectedRepo     : Option[Repository]  = None,
    selectedProvider : Option[Provider]    = None,
    cloneProgress    : Option[GitProgress] = None
  )

  class Backend($: BackendScope[Props, State]) {

    def subscribeToProgress: Callback = Callback {
      Events.gitProgress
        .throttleLast(500.millis)
        .doWork { p =>
          $.modState(_.copy(cloneProgress = Some(p))).runNow()
        }
        .subscribe()
    }

    def onClickLoad(e: ReactMouseEvent): Callback =
      e.preventDefaultCB >>
      subscribeToProgress >>
      $.props.zip($.state) flatMap { case (props, state) =>
        state.selectedRepo
          .map(props.onSelect(_))
          .getOrElse(Callback.empty)
      }

    def onSelectProvider(provider: Provider): Callback =
      $.modState(_.copy(selectedProvider = Some(provider)))

    def onSelectRepo(repo: Repository): Callback =
      $.modState(_.copy(
        selectedRepo  = Some(repo),
        cloneProgress = None
      ))

    def onRequestHide: Callback =
      Actions dispatchCB ToggleLoadRepoModal(false)

    val cancelButton =
      <.button(
        ^.className := "btn",
        ^.onClick  --> onRequestHide,
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
      Modal(onRequestHide)(
        <.div(^.className := "modal-header with-nav-tabs",
          Modal.closeButton(onRequestHide),
          <.h3("Load a Git repository")
        ),
        <.div(^.className := "modal-body with-nav-tabs",
          renderBody(props, state)
        ),
        renderFooter(props, state)
      )

    def renderBody(props: Props, state: State) = props.repos match {
      case None =>
        <.div(loading)

      case Some(repos) =>
        <.div(^.className := "panel with-nav-tabs panel-default",
          <.div(^.className := "panel-heading",
            renderProviders(repos.keys.toSeq, state.selectedProvider)
          ),
          <.div(^.className := "panel-body",
            state.selectedProvider.isDefined ?=
              renderRepositories(
                repos(state.selectedProvider.get),
                state.selectedRepo,
                props.cloning
              )
            )
        )
    }

    def renderProviders(providers: Seq[Provider], selected: Option[Provider]) =
      <.ul(^.className := "nav nav-tabs",
        providers.map { p =>
          <.li(
            ^.classSet1(
              s"provider-${p.id}",
              "active" -> selected.exists(_ == p)
            ),
            <.a(
              ^.onClick --> onSelectProvider(p),
              p.name
            )
          )
        }
      )

    def renderRepositories(
      repos: Seq[Repository],
      selectedRepo: Option[Repository],
      cloning: Boolean
    ) =
      <.div(
        RepositoryList(
          repos    = repos,
          onSelect = onSelectRepo,
          selected = selectedRepo,
          disabled = cloning
        )
      )

    def renderFooter(props: Props, state: State) = {
      val cloneInProgress = props.cloning && state.cloneProgress.isDefined

      <.div(^.className := "modal-footer",
        cloneInProgress              ?= renderCloneProgress(state.cloneProgress.get),
        !props.cloning               ?= cancelButton,
        state.selectedRepo.isDefined ?= loadButton(props.cloning)
      )
    }

    def renderCloneProgress(cp: GitProgress) =
      <.span(^.className := "clone-progress",
        s"${cp.taskName}",
        cp.percentage.map(p => <.span(s" ($p%)")).getOrElse(EmptyTag)
      )
  }

  val component =
    ReactComponentB[Props]("LoadRepositoryModal")
      .initialState_P(props =>
        State(selectedProvider = props.providers.headOption)
      )
      .renderBackend[Backend]
      .build

  def apply(
    onSelect: Repository => Callback,
    loading: Boolean = false,
    repos: Option[Map[Provider, Seq[Repository]]] = None
  ) = component(Props(onSelect, loading, repos))

}

