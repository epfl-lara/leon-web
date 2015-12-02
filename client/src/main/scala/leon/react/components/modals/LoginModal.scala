/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react
package components
package modals

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import leon.web.client.react.attrs._
import leon.web.client.syntax.Observer._

/** Inform the user of what is about to happen when they click the 'Login' button.
  * Redirect the user to `/login` once they do.
  */
object LoginModal {

  case class State(processing: Boolean = false)
  case class Props(isOpen: Boolean = false)

  class Backend($: BackendScope[Props, State]) {

    val closeButton =
      <.button(
        ^.className := "btn",
        ^.onClick  --> onClose,
        dataDismiss := "modal",
        "Close"
      )

    def loginButton(processing: Boolean) =
      <.a(
        ^.className := "btn btn-primary",
        ^.onClick  --> onLogin,
        ^.role      := "button",
        ^.href      := "/login",
        ^.disabled  := processing,
        if (processing) "Logging in…" else "Login"
      )

    def onClose: Callback =
      $.modState(_.copy(processing = false)) >>
      Callback { Actions.toggleLoginModal ! ToggleLoginModal(false) }

    def onLogin: Callback =
      $.modState(_.copy(processing = true))

    def render(props: Props, state: State) =
      Modal(props.isOpen)(
        <.div(^.className := "modal-header",
          Modal.closeButton(onClose),
          <.h3("Login with GitHub")
        ),
        <.div(^.className := "modal-body",
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
        <.div(^.className := "modal-footer",
          if (!state.processing) closeButton else EmptyTag,
          loginButton(state.processing)
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

