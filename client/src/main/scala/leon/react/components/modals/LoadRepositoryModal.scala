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
import leon.web.client.HandlersTypes.HRepository

import monifu.concurrent.Implicits.globalScheduler

/** Allows users to pick a repository in a list, and clone/pull it.
  * Also displays the current state of the clone/pull operation.
  */
object LoadRepositoryModal {

  case class Props(
    onSelect: HRepository => Callback,
    cloning: Boolean = false,
    repos: Option[Seq[HRepository]] = None
  )

  case class State(
    selectedRepo  : Option[HRepository] = None,
    url           : Option[String]      = None,
    cloneProgress : Option[GitProgress] = None
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

    def urlToRepo(url: GitHubURL): HRepository = new HRepository {
      val id            = 0L
      val name          = url.repo
      val fullName      = url.repopath
      val owner         = url.user
      val visibility    = ""
      val fork          = false
      val size          = 0L
      val cloneURL      = s"https://github.com/${url.repopath}.git"
      val defaultBranch = "master"
      val branches      = new js.Array[String]
    }

    def repoToURL(repo: HRepository): String =
      s"https://github.com/${repo.fullName}.git"

    def onChangeURL(e: ReactEventI): Callback = {
      val url   = e.target.value
      val ghUrl = GitHubURL.parse(url)

      $.modState(_.copy(url = Some(url))) >> CallbackTo.sequenceO {
        ghUrl.map(urlToRepo).map { repo =>
          $.modState(_.copy(
            selectedRepo  = Some(repo),
            cloneProgress = None
          ))
        }
      }
    }.void

    def onSelectRepo(repo: HRepository): Callback = {
      $.modState(_.copy(
        selectedRepo  = Some(repo),
        url           = Some(repoToURL(repo)),
        cloneProgress = None
      ))
    }

    def onRequestHide: Callback = Callback {
      Actions dispatch ToggleLoadRepoModal(false)
    }

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
        <.div(^.className := "modal-header",
          Modal.closeButton(onRequestHide),
          <.h3("Load a repository from GitHub")
        ),
        <.div(^.className := "modal-body",
          renderURLForm(state.selectedRepo, state.url),
          renderRepositoriesList(props.repos, state.selectedRepo, props.cloning)
        ),
        renderFooter(props, state)
      )

    def renderURLForm(repo: Option[HRepository], url: Option[String]) = {
      <.div(^.className := "modal-section",
        <.h5(
          """Enter the URL of a GitHub repository:"""
        ),
        <.input(
          ^.`type`      := "text",
          ^.className   := "form-control",
          ^.placeholder := "https://github.com/user/reponame",
          ^.value       := url.getOrElse(""),
          ^.onChange   ==> onChangeURL
        )
      )
    }

    def renderRepositoriesList(repos: Option[Seq[HRepository]], selected: Option[HRepository], cloning: Boolean) =
      <.div(^.className := "modal-section",
        <.h5(
          """Or pick a repository to load from the list below:"""
        ),
        repos match {
          case None        => loading
          case Some(repos) =>
            RepositoryList(
              repos    = repos,
              onSelect = onSelectRepo,
              selected = selected,
              disabled = cloning
            )
        }
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
    loading: Boolean = false,
    repos: Option[Seq[HRepository]] = None
  ) = component(Props(onSelect, loading, repos))

}

