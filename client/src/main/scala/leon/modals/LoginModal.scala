package leon.web.client
package modals

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import monifu.concurrent.Implicits.globalScheduler
import monifu.concurrent.Cancelable
import monifu.reactive.Observable

import leon.web.client.bootstrap.Modal
import leon.web.client.react.attrs._

object LoginModal {

  val closeButton =
    <.button(
      ^.`class`   := "btn",
      dataDismiss := "modal",
      "Close"
    )

  val loginButton =
    <.a(
      ^.`class` := "btn btn-primary",
      ^.role    := "button",
      ^.href    := "/login",
      "Login"
    )

  case class Props(events: Observable[Modal.Command])

  val component =
    ReactComponentB[Props]("LoginModal")
      .render(props => {
        Modal(props.events)(
          <.div(^.`class` := "modal-header",
            Modal.closeButton,
            <.h3("Login with GitHub")
          ),
          <.div(^.`class` := "modal-body",
            """
            When logging in with your GitHub account, you will be asked to give
            Leon access to your public and possibly private repositories.
            You will then be able to load files from any repository you
            gave Leon access to. Click the Login button below to start
            the authentication process.
            """
          ),
          <.div(^.`class` := "modal-footer",
            loginButton,
            closeButton
          )
        )
      })
      .build

  def apply(events: Observable[Modal.Command]) = component(Props(events))

}

