/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react
package components
package git

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

/** git diff view */
object DiffView {

  case class Props(diff: String)

  class Backend($: BackendScope[Props, Unit]) {
    def render(props: Props) = {
      <.div(^.className := "git-diff-view",
        <.pre(
          props.diff
        )
      )
    }
  }

  val component =
    ReactComponentB[Props]("Git.DiffView")
      .stateless
      .renderBackend[Backend]
      .build

  def apply(diff: String) =
    component(Props(diff))
}

