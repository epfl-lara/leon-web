package leon.web.client
package components
package modals

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

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

  val nop = () => {};

  case class Props(isOpen: Boolean = false)

  class Backend($: BackendScope[Props, Unit]) {
    def render(props: Props) =
      Modal(props.isOpen)(
        <.div(^.`class` := "modal-header",
          Modal.closeButton,
          <.h3("Login with GitHub")
        ),
        <.div(^.`class` := "modal-body",
          <.p(
            """
            When logging in with your GitHub account, you will be asked to give
            Leon access to your public and possibly private repositories.
            You will then be able to load files from any repository you
            gave Leon access to. Note that you can revoke this access at
            any time from
            """,
            <.a(^.href := "https://github.com/settings/applications", "your GitHub settings"),
            "."
          ),
          <.p(
            """Click the Login button below to start the authentication process."""
          )
        ),
        <.div(^.`class` := "modal-footer",
          loginButton,
          closeButton
        )
      )
  }

  val component =
    ReactComponentB[Props]("LoginModal")
      .renderBackend[Backend]
      .build

  def apply(isOpen: Boolean = false) = component(Props(isOpen))

}

