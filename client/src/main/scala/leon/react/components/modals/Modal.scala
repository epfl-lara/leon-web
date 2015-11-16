/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web.client
package react
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

/** Generic modal component.
  *
  * {{
  * import japgolly.scalajs.react._
  * import japgolly.scalajs.react.vdom.prefix_<^._
  * import leon.web.client.react.components.modals.Modal
  *
  * def render = <.Modal(isOpen = true) (
  *   <.div(^.className := "modal-header",
  *     Modal.closeButton,
  *     <.h3("Load a repository from GitHub")
  *   ),
  *   <.div(^.className := "modal-body",
  *     <.p(
  *       """Pick a repository to load from the list below:"""
  *     ),
  *     // etc.
  *   ),
  *   <.div(^.className := "modal-footer",
  *     cancelButton, loadButton
  *   )
  * )
  * }}
  */
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

    def onMount: Callback = $.props.map { props =>
      if (props.isOpen) show() else hide()
    }

    def onUnmount: Callback = Callback {
      hide()
    }

    def onUpdate(prevProps: Props): Callback =
      onMount

    def render(children: PropsChildren) =
      <.div(
        ^.ref          := ref,
        ^.className    := "modal fade nice-modal",
        ^.role         := "dialog",
        ariaHidden     := "true",
        dataBackdrop   := "false",
        <.div(
          ^.className := "modal-dialog",
          <.div(
            ^.className := "modal-content",
            children
          )
        )
      )
  }

  val component =
    ReactComponentB[Props]("Modal")
      .renderBackend[Backend]
      .componentDidMount(_.backend.onMount)
      .componentWillUnmount(_.backend.onUnmount)
      .componentDidUpdate(scope => scope.$.backend.onUpdate(scope.prevProps))
      .build

  def apply(isOpen: Boolean)(children: ReactNode*) =
    component(Props(isOpen), children: _*)

  val closeButton =
    <.button(
      ^.`type`    := "button",
      ^.className := "close",
      dataDismiss := "modal",
      ariaHidden  := "true",
      "×"
    )

}

