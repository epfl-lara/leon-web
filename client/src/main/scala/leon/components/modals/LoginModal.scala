package leon.web.client
package components
package modals

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import leon.web.client.react.attrs._

object LoginModal {

  val nop = () => {};

  case class State(processing: Boolean = false)
  case class Props(isOpen: Boolean = false)

  class Backend($: BackendScope[Props, State]) {

    val closeButton =
      <.button(
        ^.`class`   := "btn",
        ^.onClick --> onClose,
        dataDismiss := "modal",
        "Close"
      )

    def loginButton(processing: Boolean) =
      <.a(
        ^.`class` := "btn btn-primary",
        ^.onClick --> onLogin,
        ^.role    := "button",
        ^.href    := "/login",
        if (processing) "Logging in…" else "Login"
      )

    def onClose: Callback = $.modState(_.copy(processing = false))
    def onLogin: Callback = $.modState(_.copy(processing = true))

    def render(props: Props, state: State) =
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
          loginButton(state.processing),
          closeButton
        )
      )
  }

  val component =
    ReactComponentB[Props]("LoginModal")
      .initialState(State())
      .renderBackend[Backend]
      .build

  def apply(isOpen: Boolean = false) = component(Props(isOpen))

}

