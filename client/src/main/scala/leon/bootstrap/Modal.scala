package leon.web.client
package bootstrap

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
  case class Props(events: Observable[Command])

  val ref = Ref[HTMLDivElement]("modal")

  class Backend($: BackendScope[Props, State]) {
    def show(): Unit = modal(Show)
    def hide(): Unit = modal(Hide)

    def modal(cmd: Command): Unit = {
      ref($).toOption.map(_.getDOMNode) match {
        case Some(el) => jQuery(el).modal(cmd.toString)
        case None     => println("Modal.Backend.modal(): Cannot find ref")
      }
    }

    var subscription: Option[Cancelable] = None

    def onMount(): Unit = {
      val cancel = $.props.events.doWork(onEvent).subscribe()
      subscription = Some(cancel)
    }

    def onUnmount(): Unit = {
      subscription.foreach(_.cancel())
    }

    def onEvent(cmd: Command): Unit =
      modal(cmd)
  }

  val component =
    ReactComponentB[Props]("Modal")
      .initialState(State())
      .backend(new Backend(_))
      .render($ =>
        <.div(
          ^.ref          := ref,
          ^.`class`      := "modal fade",
          ^.role         := "dialog",
          ariaHidden     := "true",
          dataBackdrop   := "static",
          <.div(
            ^.`class` := "modal-dialog",
            <.div(
              ^.`class` := "modal-content",
              $.propsChildren
            )
          )
        )
      )
      .componentDidMount(_.backend.onMount())
      .componentWillUnmount(_.backend.onUnmount())
      .build

  type Constructor = Channel => ReactComponentU[_ , Unit, Unit, Element]

  def apply(events: Observable[Command])(children: ReactNode*) =
    component(Props(events), children)

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

