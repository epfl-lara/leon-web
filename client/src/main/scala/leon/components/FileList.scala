package leon.web.client
package components

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

object FileList {

  type OnSelectCallback = String => Callback

  case class Props(files: Seq[String], onSelect: OnSelectCallback)

  class Backend($: BackendScope[Props, Unit]) {

    def onSelectFile(e: ReactEventI): Callback =
      e.preventDefaultCB >>
      $.props.flatMap(_.onSelect(e.target.value))

    val header =
      <.option(^.value := "", "-- Select a file --")

    val noFilesFound =
      <.option(
        ^.value    := "",
        ^.key      := "no-files-found",
        ^.disabled := "true",
        "No .scala files found"
      )

    def fileOptions(files: Seq[String]) = files match {
      case Seq() => Seq(noFilesFound)
      case _     => files map { file =>
        <.option(^.value := file, ^.key := file, file)
      }
    }

    val classNames =
      "file-list form-control panel-element-full"

    def render(props: Props) = {
      val options = header +: fileOptions(props.files)

      <.select(
        ^.className := classNames,
        ^.onChange ==> onSelectFile,
        options
      )
    }
  }

  val component =
    ReactComponentB[Props]("FileList")
      .renderBackend[Backend]
      .build

  def apply(files: Seq[String], onSelect: OnSelectCallback) =
    component(Props(files, onSelect))

}

