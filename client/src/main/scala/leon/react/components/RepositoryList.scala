/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react
package components

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import leon.web.client.HandlersTypes.HRepository

/** List of GitHub repository, matching the GitHub
  * color scheme (eg. yellow background for private repos)
  * and features pretty octicons. */
object RepositoryList {

  type OnSelectCallback = HRepository => Callback

  case class Props(
    repos: Seq[HRepository],
    onSelect: OnSelectCallback,
    disabled: Boolean
  )

  case class State(selected: Option[HRepository] = None)

  class Backend($: BackendScope[Props, State]) {

    def onSelectRepo(repo: HRepository)(e: ReactMouseEvent): Callback =
      e.preventDefaultCB >>
      $.modState(_.copy(selected = Some(repo))) >>
      $.props.flatMap(_.onSelect(repo))

    def render(props: Props, state: State) =
      <.ul(
        ^.classSet1(
          "repository-list",
          "disabled" -> props.disabled
        ),
        for (repo <- props.repos) yield
          <.li(
            ^.classSet1(
              repo.visibility,
              "selected" -> state.selected.exists(_ == repo)
            ),
            <.a(^.onClick ==> onSelectRepo(repo) _,
              <.span(^.classSet1(
                "octicon",
                "octicon-repo"        -> !repo.fork,
                "octicon-repo-forked" -> repo.fork
              )),
              repo.fullName
            )
        )
      )
  }

  val component =
    ReactComponentB[Props]("RepositoryList")
      .initialState(State())
      .renderBackend[Backend]
      .build

  def apply(repos: Seq[HRepository],
            onSelect: OnSelectCallback,
            disabled: Boolean) =
    component(Props(repos, onSelect, disabled))

}

