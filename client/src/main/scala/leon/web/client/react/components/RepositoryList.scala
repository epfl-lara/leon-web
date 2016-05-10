/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react
package components

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import leon.web.shared.messages.HRepository

/** List of GitHub repository, matching the GitHub
  * color scheme (eg. yellow background for private repos)
  * and features pretty octicons. */
object RepositoryList {

  type OnSelectCallback = HRepository => Callback

  case class Props(
    repos: Seq[HRepository],
    selected: Option[HRepository],
    onSelect: OnSelectCallback,
    disabled: Boolean
  )

  class Backend($: BackendScope[Props, Unit]) {

    def onSelectRepo(repo: HRepository)(e: ReactMouseEvent): Callback =
      e.preventDefaultCB >>
      $.props.flatMap(_.onSelect(repo))

    def render(props: Props) =
      <.ul(
        ^.classSet1(
          "repository-list",
          "disabled" -> props.disabled
        ),
        for (repo <- props.repos) yield
          <.li(
            ^.classSet1(
              repo.visibility,
              "selected" -> props.selected.exists(_.fullName === repo.fullName)
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
      .renderBackend[Backend]
      .build

  def apply(repos: Seq[HRepository],
            selected: Option[HRepository],
            onSelect: OnSelectCallback,
            disabled: Boolean) =
    component(Props(repos, selected, onSelect, disabled))

}

