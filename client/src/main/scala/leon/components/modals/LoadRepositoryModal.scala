package leon.web.client
package components
package modals

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import leon.web.client.HandlersTypes.HRepository

import leon.web.client.react.attrs._

object LoadRepositoryModal {

  case class Props(
    onSelect: HRepository => Callback,
    isOpen: Boolean = false,
    loading: Boolean = false,
    repos: Option[Seq[HRepository]] = None
  )

  case class State(selectedRepo: Option[HRepository] = None)

  class Backend($: BackendScope[Props, State]) {

    def onSelectRepo(repo: HRepository): Callback = {
      $.modState(_.copy(selectedRepo = Some(repo)))
    }

    def onClickLoad(e: ReactMouseEvent): Callback =
      e.preventDefaultCB >>
      $.props.zip($.state) flatMap { case (props, state) =>
        state.selectedRepo
          .map(props.onSelect(_))
          .getOrElse(Callback.empty)
      }

    val cancelButton =
      <.button(
        ^.`class`   := "btn",
        dataDismiss := "modal",
        "Cancel"
      )

    def loadButton(loading: Boolean) =
      <.a(
        ^.`class` := "btn btn-primary",
        ^.role    := "button",
        ^.onClick ==> onClickLoad,
        if (loading) "Loading..." else "Load"
      )

    val loading = Spinner()

    def render(props: Props, state: State) =
      Modal(props.isOpen)(
        <.div(^.`class` := "modal-header",
          Modal.closeButton,
          <.h3("Load a repository from GitHub")
        ),
        <.div(^.`class` := "modal-body",
          <.p(
            """Pick a repository to load from the list below:"""
          ),
          props.repos match {
            case None        => loading
            case Some(repos) => RepositoryList(repos, onSelect = onSelectRepo)
          }
        ),
        <.div(^.`class` := "modal-footer",
          if (state.selectedRepo.isDefined) loadButton(props.loading) else EmptyTag,
          cancelButton
        )
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

