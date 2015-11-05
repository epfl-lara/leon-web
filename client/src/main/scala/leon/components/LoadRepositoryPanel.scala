package leon.web.client
package components

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import leon.web.client.stores.RepositoryStore
import leon.web.client.components.modals.LoadRepositoryModal
import leon.web.client.HandlersTypes.HRepository

object LoadRepositoryPanel {

  case class State(
    repos: Option[Seq[HRepository]] = None,
    repo: Option[HRepository] = None,
    files: Seq[String] = Seq(),
    file: Option[String] = None,
    openModal: Boolean = false,
    loading: Boolean = false
  )

  class Backend($: BackendScope[Unit, State]) {

    import RepositoryStore._

    def didMount: Callback = Callback {
      RepositoryStore.listen((onStoreEvent _).andThen(_.runNow()))
    }

    def onStoreEvent(e: Event): Callback = e match {
      case RepositoriesLoaded(repos) =>
        Callback.log("LoadRepositoryPanel.RepositoriesLoaded") >>
        $.modState(_.copy(repos = Some(repos)))

      case FilesLoaded(files) =>
        Callback.log("LoadRepositoryPanel.FilesLoaded") >>
        $.modState(_.copy(
          files = files,
          loading = false,
          openModal = false
        ))

      case FileLoaded(file, content) =>
        Callback.log("LoadRepositoryPanel.FileLoaded") >>
        Callback {
          RepositoryStore ! SetEditorCode(content)
        }
    }

    def loadRepos: Callback = Callback {
      RepositoryStore ! LoadRepositories()
    }

    def loadFiles(repo: HRepository): Callback = Callback {
      RepositoryStore ! LoadFiles(repo)
    }

    def loadFile(repo: HRepository, file: String): Callback = Callback {
      RepositoryStore ! LoadFile(repo, file)
    }

    def showLoadRepoModal: Callback =
      Callback.log("LoadRepositoryPanel.showLoadRepoModal") >>
      $.modState(_.copy(openModal = true, loading = false)) >>
      loadRepos

    def onLoadRepo(repo: HRepository): Callback =
      Callback.log("LoadRepositoryPanel.onLoadRepo") >>
      $.modState(_.copy(repo = Some(repo), loading = true)) >>
      loadFiles(repo)

    def onChooseFile(file: String): Callback =
      Callback.log("LoadRepositoryPanel.onChooseFile") >>
      $.modState(_.copy(file = Some(file))) >>
      $.state.flatMap { state => loadFile(state.repo.get, file) }

    def render(state: State) =
      <.div(^.`class` := "panel",
        <.h3("Load a GitHub repository:"),
        <.div(
          LoadRepositoryButton(state.repo, showLoadRepoModal),
          renderFileList(state.repo, state.files)
        ),
        <.div(
          LoadRepositoryModal(onLoadRepo, state.openModal, state.loading, state.repos)
        )
      )

    def renderFileList(repo: Option[HRepository], files: Seq[String]) = repo match {
      case None    => EmptyTag
      case Some(_) =>
        <.div(^.id := "load-repo-file",
          FileList(files, onChooseFile)
        )
    }
  }

  val component =
    ReactComponentB[Unit]("LoadRepositoryPanel")
      .initialState(State())
      .renderBackend[Backend]
      .componentDidMount(_.backend.didMount)
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

