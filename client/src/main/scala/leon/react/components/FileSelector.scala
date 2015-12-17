/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react
package components

import japgolly.scalajs.react._

/** Simple file selector */
object FileSelector {

  case class Props(
    files: Seq[String],
    onSelect: String => Callback,
    selected: Option[String] = None
  )

  class Backend($: BackendScope[Props, Unit]) {

    def render(props: Props) = {
      Dropdown(
        className    = "file-list panel-element-full",
        items        = props.files,
        header       = Some("-- Select a file --"),
        empty        = Some("No .scala files found"),
        renderOption = identity[String],
        onSelect     = props.onSelect,
        selected     = props.selected
      )
    }
  }

  val component =
    ReactComponentB[Props]("FileSelector")
      .renderBackend[Backend]
      .build

  def apply(files: Seq[String], onSelect: String => Callback, selected: Option[String] = None) =
    component(Props(files, onSelect, selected))

}

