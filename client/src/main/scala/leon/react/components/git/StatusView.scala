/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react
package components
package git

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

/** git status view */
object StatusView {

  case class Props(status: Map[String, Set[String]])

  class Backend($: BackendScope[Props, Unit]) {

    val sections = List(
      "added"     -> "Added",
      "changed"   -> "Changed",
      "modified"  -> "Modified",
      "removed"   -> "Removed",
      "untracked" -> "Untracked"
    )

    def render(props: Props) = {
      def status(key: String): Set[String] =
        props.status.getOrElse(key, Set.empty[String])

      <.div(^.className := "git-status-view",
        sections
          .map { case (id, name) => (id, name, status(id)) }
          .filter(_._3.nonEmpty)
          .map(Function.tupled(section))
      )
    }

    def section(id: String, header: String, files: Set[String]) = {
      <.div(^.classSet1("git-status-section", id -> true),
        <.h5(header),
        <.ul(^.className := "git-status-files",
          files.toList.sorted.map(<.li(_))
        )
      )
    }
  }

  val component =
    ReactComponentB[Props]("Git.StatusView")
      .renderBackend[Backend]
      .build

  def apply(status: Map[String, Set[String]]) =
    component(Props(status))

}

