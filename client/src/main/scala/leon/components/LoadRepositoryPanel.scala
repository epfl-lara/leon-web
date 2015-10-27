package leon.web.client
package components

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import leon.web.client.components.modals.LoadRepositoryModal
import leon.web.client.HandlersTypes.HRepository

object LoadRepositoryPanel {

  case class State(
    repos: Option[Seq[HRepository]] = None,
    repo: Option[HRepository] = None,
    files: Seq[String] = Seq(),
    openModal: Boolean = false
  )

  class Backend($: BackendScope[Unit, State]) {

    import RepositoryStore._

    def didMount(): Callback = Callback {
      RepositoryStore.listen((onStoreEvent _).andThen(_.runNow()))
    }

    def onStoreEvent(e: Event): Callback = e match {
      case RepositoriesLoaded(repos) =>
        $.modState(_.copy(repos = Some(repos)))

      case FilesLoaded(files) =>
        $.modState(_.copy(files = files))
    }

    def loadRepos(): Unit =
      RepositoryStore ! LoadRepositories()

    def loadFiles(repo: HRepository): Unit =
      RepositoryStore ! LoadFiles(repo)

    def showLoadRepoModal: Callback =
      $.modState(_.copy(openModal = true)) >>
      Callback.lift(loadRepos)

    def onSelectRepo(repo: HRepository): Callback =
      $.modState(_.copy(repo = Some(repo), openModal = false)) >>
      Callback.lift(() => loadFiles(repo))

    def render(state: State) =
      <.div(^.`class` := "panel",
        <.h3("Load a GitHub repository:"),
        LoadRepositoryButton(state.repo, showLoadRepoModal),
        renderFileList(state.files),
        LoadRepositoryModal(onSelectRepo, state.openModal, state.repos)
      )

    def renderFileList(files: Seq[String]) =
      if (files.isEmpty)
        EmptyTag
      else
        <.div(^.id := "load-repo-file",
          FileList(files, x => {})
        )
  }

  val component =
    ReactComponentB[Unit]("LoadRepositoryPanel")
      .initialState(State())
      .renderBackend[Backend]
      .componentDidMount(_.backend.didMount())
      .buildU

  def apply() = component()

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
        ^.`class`     :=  "btn btn-default panel-element-full",
        ^.onClick     --> props.onClick,
        ^.onMouseOver --> onMouseOver,
        ^.onMouseOut  --> onMouseOut,
        renderContent(props.repo, state.isHover)
      )

    def renderContent(repo: Option[HRepository], isHover: Boolean) = repo match {
      case Some(repo) if !isHover =>
        <.span(
          <.span(^.`class` := "octicon octicon-mark-github"),
          repo.fullName
        )

      case Some(_) =>
        <.span("Select another repository")

      case None =>
        <.span("Select a GitHub repository")
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

