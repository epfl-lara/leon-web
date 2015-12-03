/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react
package components

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import leon.web.client.react._
import leon.web.client.react.components.modals.LoadRepositoryModal
import leon.web.client.syntax.Observer._
import leon.web.client.HandlersTypes.{HRepository, HBranch}

/** Panel displayed in the sidebar on the right, wich lets
  * users pick a repository and load a specific file from that
  * repository into the editor.
  */
object LoadRepositoryPanel {

  type Props = AppState

  class Backend($: BackendScope[Props, Unit]) {

    def showLoadRepoModal: Callback = Callback {
      Actions.toggleLoadRepoModal ! ToggleLoadRepoModal(true)
    }

    def loadRepos: Callback = Callback {
      Actions.loadRepositories ! LoadRepositories()
    }

    def onClickSelect: Callback =
      showLoadRepoModal >> loadRepos

    def onClickUnload: Callback = Callback {
      Actions.setCurrentProject ! SetCurrentProject(None)
    }

    def onLoadRepo(repo: HRepository): Callback = Callback {
      Actions.loadRepository ! LoadRepository(repo)
    }

    def onChooseBranch(repo: HRepository)(branch: String): Callback = Callback {
      Actions.switchBranch ! SwitchBranch(repo, branch)
    }

    def onChooseFile(file: String): Callback = $.props.map { props =>
      Actions.loadFile ! LoadFile(props.repository.get, file)
    }

    def render(props: Props) =
      <.div(^.className := "panel",
        <.h3("Load a GitHub repository:"),
        <.div(
          LoadRepositoryButton(props.repository, onClickSelect, onClickUnload),
          renderBranches(props.repository, props.branches, props.branch),
          renderFiles(props.repository, props.files, props.file.map(_._1))
        ),
        <.div(
          LoadRepositoryModal(
            onLoadRepo,
            props.showLoadRepoModal,
            props.isLoadingRepo,
            props.repositories
          )
        )
      )

    def renderBranches(repo: Option[HRepository], branches: Seq[HBranch], selected: Option[String]) = repo match {
      case None => EmptyTag
      case Some(repo) =>
        <.div(^.id := "load-repo-branch",
          BranchSelector(
            branches.map(_.name),
            onChooseBranch(repo),
            selected orElse Some(repo.defaultBranch)
          )
        )
    }

    def renderFiles(repo: Option[HRepository], files: Seq[String],
                       selected: Option[String] = None) = repo match {

      case None => EmptyTag
      case Some(_) =>
        <.div(^.id := "load-repo-file",
          FileSelector(files, onChooseFile, selected)
        )
    }
  }

  val component =
    ReactComponentB[Props]("LoadRepositoryPanel")
      .renderBackend[Backend]
      .build

  def apply(props: Props) = component(props)

}

object LoadRepositoryButton {

  case class Props(repo: Option[HRepository], onClickSelect: Callback, onClickUnload: Callback)
  case class State(isHover: Boolean = false)

  class Backend($: BackendScope[Props, State]) {

    def onMouseOver: Callback =
      $.modState(_.copy(isHover = true))

    def onMouseOut: Callback =
      $.modState(_.copy(isHover = false))

    def render(props: Props, state: State) =
      <.div(
        <.button(
          ^.id := "load-repo-btn",
          ^.classSet1(
            "btn btn-default panel-element-full",
            "loaded" -> props.repo.isDefined
          ),
          ^.onClick     --> props.onClickSelect,
          ^.onMouseOver --> onMouseOver,
          ^.onMouseOut  --> onMouseOut,
          renderContent(props.repo, state.isHover)
        ),
        props.repo.isDefined ?= renderUnloadButton(props.onClickUnload)
      )

    def octicon(name: String, content: String) = <.span(
      <.span(^.className := s"octicon octicon-mark-$name"),
      content
    )

    def renderContent(repo: Option[HRepository], isHover: Boolean) = repo match {
      case Some(repo) if !isHover => octicon("github", repo.fullName)
      case Some(_)                => octicon("github", "Select another repository")
      case None                   => octicon("github", "Select a GitHub repository")
    }

    def renderUnloadButton(onClick: Callback) =
      <.button(
        ^.id        := "unload-repo-btn",
        ^.className := "btn btn-default",
        ^.onClick  --> onClick,
        "x"
      )

  }

  val component =
    ReactComponentB[Props]("LoadRepositoryButton")
      .initialState(State())
      .renderBackend[Backend]
      .build

  def apply(repo: Option[HRepository], onClickSelect: Callback, onClickUnload: Callback) =
    component(Props(repo, onClickSelect, onClickUnload))

}

