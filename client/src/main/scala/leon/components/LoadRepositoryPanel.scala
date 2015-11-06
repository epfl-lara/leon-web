package leon.web.client
package components

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import leon.web.client.react.AppState
import leon.web.client.actions._
import leon.web.client.syntax.Observer._
import leon.web.client.components.modals.LoadRepositoryModal
import leon.web.client.HandlersTypes.HRepository

object LoadRepositoryPanel {

  type Props = AppState

  class Backend($: BackendScope[Props, Unit]) {

    def loadRepos: Callback = Callback {
      Actions.loadRepositories ! LoadRepositories()
    }

    def loadFiles(repo: HRepository): Callback = Callback {
      Actions.loadFiles ! LoadFiles(repo)
    }

    def loadFile(repo: HRepository, file: String): Callback = Callback {
      Actions.loadFile ! LoadFile(repo, file)
    }

    def showLoadRepoModal: Callback = Callback {
      Actions.toggleLoadRepoModal ! ToggleLoadRepoModal(true)
    }

    def onClickSelect: Callback =
      showLoadRepoModal >> loadRepos

    def onLoadRepo(repo: HRepository): Callback =
      loadFiles(repo)

    def onChooseFile(file: String): Callback = $.props.flatMap { props =>
      loadFile(props.repository.get, file)
    }

    def render(props: Props) =
      <.div(^.className := "panel",
        <.h3("Load a GitHub repository:"),
        <.div(
          LoadRepositoryButton(props.repository, onClickSelect),
          renderFileList(props.repository, props.files)
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

    def renderFileList(repo: Option[HRepository], files: Seq[String]) = repo match {
      case None => EmptyTag
      case Some(_) =>
        <.div(^.id := "load-repo-file",
          FileList(files, onChooseFile)
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

  case class Props(repo: Option[HRepository], onClick: Callback)
  case class State(isHover: Boolean = false)

  class Backend($: BackendScope[Props, State]) {

    def onMouseOver: Callback =
      $.modState(_.copy(isHover = true))

    def onMouseOut: Callback =
      $.modState(_.copy(isHover = false))

    def render(props: Props, state: State) =
      <.button(
        ^.id := "load-repo-btn",
        ^.className   :=  "btn btn-default panel-element-full",
        ^.onClick     --> props.onClick,
        ^.onMouseOver --> onMouseOver,
        ^.onMouseOut  --> onMouseOut,
        renderContent(props.repo, state.isHover)
      )

    def renderContent(repo: Option[HRepository], isHover: Boolean) = repo match {
      case Some(repo) if !isHover =>
        <.span(
          <.span(^.className := "octicon octicon-mark-github"),
          repo.fullName
        )

      case Some(_) => <.span("Select another repository")
      case None    => <.span("Select a GitHub repository")
    }

  }

  val component =
    ReactComponentB[Props]("LoadRepositoryButton")
      .initialState(State())
      .renderBackend[Backend]
      .build

  def apply(repo: Option[HRepository], onClick: Callback) =
    component(Props(repo, onClick))

}

