/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
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
  * def render = Modal(onRequestHide)(
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

  case class Props(onRequestHide: Callback)

  val ref = Ref[HTMLDivElement]("modal")

  class Backend($: BackendScope[Props, Unit]) {

    def modal(cmd: Command): Callback = Callback {
      ref($).toOption match {
        case Some(el) => jQuery(el).modal(cmd.toString)
        case None     => println("Modal.Backend.modal(): Cannot find ref")
      }
    }

    def show(): Callback = modal(Show)
    def hide(): Callback = $.props flatMap { props =>
      modal(Hide) >> props.onRequestHide
    }

    def onMount: Callback =
      show()

    def onUnmount: Callback =
      hide()

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

  def apply(onRequestHide: Callback = Callback.empty)(children: ReactNode*) =
    component(Props(onRequestHide), children: _*)

  def closeButton(onRequestHide: Callback = Callback.empty) =
    <.button(
      ^.`type`    := "button",
      ^.className := "close",
      ^.onClick --> onRequestHide,
      dataDismiss := "modal",
      ariaHidden  := "true",
      "×"
    )
}

