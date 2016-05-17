/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react
package components
package git

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

/** git commit message view */
object CommitMessageView {

  case class Props(message: String, onChange: String => Callback)

  class Backend($: BackendScope[Props, Unit]) {

    def onChange(e: ReactEventI): Callback = $.props flatMap { props =>
      props.onChange(e.target.value)
    }

    def render(props: Props) = {
      <.textarea(
        ^.className   := "git-commit-msg form-control",
        ^.placeholder := "Add a commit message (mandatory)",
        ^.value       := props.message,
        ^.rows        := 4,
        ^.onChange   ==> onChange
      )
    }

  }

  val component =
    ReactComponentB[Props]("Git.CommitMessageView")
      .stateless
      .renderBackend[Backend]
      .build

  def apply(message: String, onChange: String => Callback) =
    component(Props(message, onChange))

}

