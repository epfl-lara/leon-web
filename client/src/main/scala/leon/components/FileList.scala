package leon.web.client
package components

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

object FileList {

  type OnSelectCallback = String => Unit

  case class Props(files: Seq[String], onSelect: OnSelectCallback)

  class Backend($: BackendScope[Props, Unit]) {

    def onSelectFile(e: ReactEventI): Callback =
      e.preventDefaultCB >>
      $.props.map(_.onSelect(e.target.value)).void

    val classNames = "file-list form-control panel-element-full"

    def render(props: Props) = {
      val fileOptions = props.files map { file =>
        <.option(^.value := file, ^.key := file, file)
      }

      val header = <.option(^.value := "", "-- Select a file --")
      val options = header +: fileOptions

      <.select(^.`class` := classNames, ^.onChange ==> onSelectFile, options)
    }
  }

  val component =
    ReactComponentB[Props]("FileList")
      .renderBackend[Backend]
      .build

  def apply(files: Seq[String], onSelect: OnSelectCallback) =
    component(Props(files, onSelect))

}

