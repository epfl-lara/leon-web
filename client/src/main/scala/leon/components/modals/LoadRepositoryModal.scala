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
    cloning: Boolean = false,
    repos: Option[Seq[HRepository]] = None
  )

  case class State(selectedRepo: Option[HRepository] = None)

  class Backend($: BackendScope[Props, State]) {

    def onSelectRepo(repo: HRepository): Callback = {
      $.modState(_.copy(selectedRepo = Some(repo)))
    }

    def onClickClone(e: ReactMouseEvent): Callback =
      e.preventDefaultCB >>
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
        ^.onClick  ==> onClickClone,
        if (cloning) "Cloning..." else "Load"
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
        <.div(^.className := "modal-footer",
          onlyIf(!props.cloning)(cancelButton),
          onlyIf(state.selectedRepo.isDefined)(loadButton(props.cloning))
        )
      )
  }

  def onlyIf(cond: Boolean)(el: => TagMod): TagMod = if (cond) el else EmptyTag

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

