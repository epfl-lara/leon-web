package leon.web.client
package components

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

object Spinner {

  val component =
    ReactComponentB[Unit]("Spinner")
      .render(_ =>
      <.div(^.className := "spinner",
        <.div(^.className := "bounce1"),
        <.div(^.className := "bounce2"),
        <.div(^.className := "bounce3")))
      .buildU

    def apply() = component()

}

