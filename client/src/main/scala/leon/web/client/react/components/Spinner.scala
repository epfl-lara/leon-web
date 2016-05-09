/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react
package components

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

/** Pure CSS loading spinner.
 *
 *  @url http://tobiasahlin.com/spinkit/
 */
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

