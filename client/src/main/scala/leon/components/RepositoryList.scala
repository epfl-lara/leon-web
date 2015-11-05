package leon.web.client
package components

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import leon.web.client.HandlersTypes.HRepository

object RepositoryList {

  type OnSelectCallback = HRepository => Callback

  case class Props(repos: Seq[HRepository], onSelect: OnSelectCallback)
  case class State(selected: Option[HRepository] = None)

  class Backend($: BackendScope[Props, State]) {

    def onSelectRepo(repo: HRepository)(e: ReactMouseEvent): Callback =
      e.preventDefaultCB >>
      $.modState(_.copy(selected = Some(repo))) >>
      $.props.map(_.onSelect(repo).runNow()).void

    def render(props: Props, state: State) =
      <.ul(^.`class` := "repository-list",
        for (repo <- props.repos) yield
          <.li(^.`class` := classNamesFor(repo, state.selected),
            <.a(^.onClick ==> onSelectRepo(repo) _,
              <.span(^.`class` := iconFor(repo)),
              repo.fullName
            )
        )
      )

    def classNamesFor(repo: HRepository, selected: Option[HRepository]): String = {
      val sel  = selected.filter(_ == repo).map(_ => "selected").getOrElse("")
      val priv = repo.visibility
      s"$priv $sel"
    }

    def iconFor(repo: HRepository): String = {
      val forked = if (repo.fork) "-forked" else ""
      s"octicon octicon-repo$forked"
    }
  }

  val component =
    ReactComponentB[Props]("RepositoryList")
      .initialState(State())
      .renderBackend[Backend]
      .build

  def apply(repos: Seq[HRepository], onSelect: OnSelectCallback) =
    component(Props(repos, onSelect))

}

