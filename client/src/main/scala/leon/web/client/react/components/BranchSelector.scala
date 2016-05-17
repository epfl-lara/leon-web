/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react
package components

import japgolly.scalajs.react._

/** Simple branch selector */
object BranchSelector {

  case class Props(
    branches: Seq[String],
    onSelect: String => Callback,
    selected: Option[String] = None
  )

  class Backend($: BackendScope[Props, Unit]) {

    def render(props: Props) = {
      Dropdown(
        className    = "branch-list panel-element",
        items        = props.branches,
        header       = None,
        empty        = Some("No branches found"),
        renderOption = identity[String],
        onSelect     = props.onSelect,
        selected     = props.selected
      )
    }
  }

  val component =
    ReactComponentB[Props]("BranchSelector")
      .renderBackend[Backend]
      .build

  def apply(branches: Seq[String], onSelect: String => Callback, selected: Option[String] = None) =
    component(Props(branches, onSelect, selected))

}

