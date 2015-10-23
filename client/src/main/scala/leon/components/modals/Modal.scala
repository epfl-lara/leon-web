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

import monifu.concurrent.Implicits.globalScheduler
import monifu.concurrent.Cancelable
import monifu.reactive.Observable
import monifu.reactive.subjects.PublishSubject

import leon.web.client.react.attrs._

object Modal {

  sealed trait Command
  case object Show extends Command {
    override def toString = "show"
  }
  case object Hide extends Command {
    override def toString = "hide"
  }

  case class State()
  case class Props(
    events: Observable[Command],
    onShow: () => Unit,
    onHide: () => Unit
  )

  val ref = Ref[HTMLDivElement]("modal")

  class Backend($: BackendScope[Props, State]) {
    def show(): Unit = modal(Show)
    def hide(): Unit = modal(Hide)

    def modal(cmd: Command): Unit = {
      ref($).toOption match {
        case Some(el) => jQuery(el).modal(cmd.toString)
        case None     => println("Modal.Backend.modal(): Cannot find ref")
      }
    }

    var subscription: Option[Cancelable] = None

    def onMount() = $.props map { props =>
      val cancel = props.events.doWork(onEvent).subscribe()
      subscription = Some(cancel)
    }

    def onUnmount() = Callback {
      subscription.foreach(_.cancel())
    }

    def onEvent(cmd: Command) = cmd match {
      case Show =>
        modal(Show)
        $.props.map(_.onShow()).runNow()

      case Hide =>
        modal(Hide)
        $.props.map(_.onHide()).runNow()
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
      .initialState(State())
      .renderBackend[Backend]
      .componentDidMount(_.backend.onMount())
      .componentWillUnmount(_.backend.onUnmount())
      .build

  type Constructor = Channel => ReactComponentU[_ , _, _, Element]

  def apply(events: Observable[Command], onShow: () => Unit, onHide: () => Unit)
           (children: ReactNode*) =
    component(Props(events, onShow, onHide), children)

  def apply() = component

  type Channel = PublishSubject[Command]
  def channel(): Channel = PublishSubject[Command]()

  val closeButton =
    <.button(
      ^.`type`    := "button",
      ^.`class`   := "close",
      dataDismiss := "modal",
      ariaHidden  := "true",
      "×"
    )

}

