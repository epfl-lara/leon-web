package leon.web.client
package components

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

object Spinner {

  val component =
    ReactComponentB[Unit]("Spinner")
      .render(_ =>
      <.div(^.`class` := "spinner",
        <.div(^.`class` := "bounce1"),
        <.div(^.`class` := "bounce2"),
        <.div(^.`class` := "bounce3")))
      .buildU

    def apply() = component()

}

