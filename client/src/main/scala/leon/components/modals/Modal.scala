package leon.web.client
package components
package modals

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import scala.scalajs.js
import org.scalajs.dom.raw.{HTMLDivElement, Element}
import org.scalajs.jquery
import jquery.jQuery
import JQueryExtended._

import leon.web.client.react.attrs._

object Modal {

  sealed trait Command
  case object Show extends Command {
    override def toString = "show"
  }
  case object Hide extends Command {
    override def toString = "hide"
  }

  case class Props(isOpen: Boolean = false)

  val ref = Ref[HTMLDivElement]("modal")

  class Backend($: BackendScope[Props, Unit]) {

    def modal(cmd: Command): Unit = {
      ref($).toOption match {
        case Some(el) => jQuery(el).modal(cmd.toString)
        case None     => println("Modal.Backend.modal(): Cannot find ref")
      }
    }

    def show(): Unit = modal(Show)
    def hide(): Unit = modal(Hide)

    def onMount() = $.props.map { props =>
      if (props.isOpen) show() else hide()
    }

    def onUpdate(prevProps: Props) = $.props map { props =>
      onMount().runNow()
    }

    def render(children: PropsChildren) =
      <.div(
        ^.ref          := ref,
        ^.`class`      := "modal fade nice-modal",
        ^.role         := "dialog",
        ariaHidden     := "true",
        dataBackdrop   := "static",
        <.div(
          ^.`class` := "modal-dialog",
          <.div(
            ^.`class` := "modal-content",
            children
          )
        )
      )
  }

  val component =
    ReactComponentB[Props]("Modal")
      .renderBackend[Backend]
      .componentDidMount(_.backend.onMount())
      .componentDidUpdate(scope => scope.$.backend.onUpdate(scope.prevProps))
      .build

  def apply(isOpen: Boolean)(children: ReactNode*) =
    component(Props(isOpen), children)

  def apply() = component

  val closeButton =
    <.button(
      ^.`type`    := "button",
      ^.`class`   := "close",
      dataDismiss := "modal",
      ariaHidden  := "true",
      "×"
    )

}

