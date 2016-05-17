/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react
package components
package panels

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import leon.web.client.react._
import leon.web.client.react.utils._
import leon.web.client.react.components.modals.LoadRepositoryModal
import leon.web.shared.github.{Repository, Branch}

/** Panel displayed in the sidebar on the right, wich lets
  * users pick a repository and load a specific file from that
  * repository into the editor.
  */
object LoadRepositoryPanel {

  type Props = AppState

  class Backend($: BackendScope[Props, Unit]) {

    def showLoadRepoModal: Callback =
      Actions dispatchCB ToggleLoadRepoModal(true)

    def loadRepos: Callback =
      Actions dispatchCB LoadRepositories()

    def onClickSelect: Callback =
      showLoadRepoModal >> loadRepos

    def onClickUnload: Callback =
      Actions dispatchCB SetCurrentProject(None)

    def onLoadRepo(repo: Repository): Callback =
      Actions dispatchCB LoadRepository(repo)

    def onChooseBranch(repo: Repository)(branch: String): Callback =
      Actions dispatchCB SwitchBranch(repo, branch)

    def onChooseFile(file: String): Callback = $.props.flatMap { props =>
      Actions dispatchCB LoadFile(props.repository.get, file)
    }

    def onChangeProjectType(e: ReactEventI): Callback =
      Actions dispatchCB SetTreatAsProject(e.target.checked)

    def render(props: Props) = {
      val hasRepo = props.repository.isDefined
      <.div(^.className := "panel",
        <.h3("Load a GitHub repository:"),
        <.div(
          LoadRepositoryButton(props.repository, onClickSelect, onClickUnload),
          hasRepo ?= renderBranches(props.repository.get, props.branches, props.branch),
          hasRepo ?= renderProjectType(props.treatAsProject),
          hasRepo ?= renderFiles(props.files, props.file.map(_._1))
        ),
        props.showLoadRepoModal ?= <.div(
          LoadRepositoryModal(
            onLoadRepo,
            props.isLoadingRepo,
            props.repositories
          )
        ),
        props.currentProject.isDefined ?= GitPanel()
      )
    }

    def renderProjectType(treatAsProject: Boolean) = {
      <.span(^.className := "project-type",
        <.input(
          ^.id        := "project-type-check",
          ^.`type`    := "checkbox",
          ^.checked   := treatAsProject,
          ^.onChange ==> onChangeProjectType
        ),
        <.label(^.`for` := "project-type-check", "Treat as project")
      )
    }

    def renderBranches(repo: Repository, branches: Seq[Branch], selected: Option[String]) =
      <.span(^.id := "load-repo-branch",
        "Branch:",
        BranchSelector(
          branches.map(_.name),
          onChooseBranch(repo),
          selected orElse Some(repo.defaultBranch)
        )
      )

    def renderFiles(files: Seq[String], selected: Option[String] = None) =
      <.div(^.id := "load-repo-file",
        FileSelector(files, onChooseFile, selected)
      )

  }

  val component =
    ReactComponentB[Props]("LoadRepositoryPanel")
      .renderBackend[Backend]
      .build

  def apply(props: Props) = component(props)

}

object LoadRepositoryButton {

  case class Props(repo: Option[Repository], onClickSelect: Callback, onClickUnload: Callback)
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

    def renderContent(repo: Option[Repository], isHover: Boolean) = repo match {
      case Some(repo) if !isHover => octicon("mark-github", repo.fullName)
      case Some(_)                => octicon("mark-github", "Select another repository")
      case None                   => octicon("mark-github", "Select a GitHub repository")
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

  def apply(repo: Option[Repository], onClickSelect: Callback, onClickUnload: Callback) =
    component(Props(repo, onClickSelect, onClickUnload))

}

