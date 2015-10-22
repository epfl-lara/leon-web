package leon.web.client
package modals

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import monifu.concurrent.Implicits.globalScheduler
import monifu.concurrent.Cancelable
import monifu.reactive.Observable

import leon.web.client.bootstrap.Modal
import leon.web.client.react.attrs._

object LoadRepositoryModal {

  val cancelButton =
    <.button(
      ^.`class`   := "btn",
      dataDismiss := "modal",
      "Cancel"
    )

  val loadButton =
    <.a(
      ^.`class` := "btn btn-primary",
      ^.role    := "button",
      "Load"
    )

  case class Props(events: Observable[Modal.Command])

  val component =
    ReactComponentB[Props]("LoadRepositoryModal")
      .render(props => {
        Modal(props.events)(
          <.div(^.`class` := "modal-header",
            Modal.closeButton,
            <.h3("Load a repository from GitHub")
          ),
          <.div(^.`class` := "modal-body",
            <.p(
              """Please pick a repository from the list below:"""
            )
          ),
          <.div(^.`class` := "modal-footer",
            loadButton,
            cancelButton
          )
        )
      })
      .build

  def apply(events: Observable[Modal.Command]) = component(Props(events))

}

