/* Copyright 2009-2016 EPFL, Lausanne */

package leon.web
package client
package react
package components
package modals

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import leon.web.client.react.attrs._
import leon.web.shared._

/** Inform the user of what is about to happen when they click the 'Login' button.
  * Redirect the user to `/login` once they do.
  */
object LoginModal {

  case class Props(user: Option[User], onRequestHide: Callback)
  case class State(loggingInWith: Option[Provider] = None)

  class Backend($: BackendScope[Props, State]) {

    def closeButton(disabled: Boolean) =
      <.button(
        ^.className := "btn",
        ^.onClick  --> onClose,
        ^.disabled  := disabled,
        dataDismiss := "modal",
        "Close"
      )

    def providerIcon(provider: Provider) = provider match {
      case Provider.GitHub  => "Login with GitHub"
      case Provider.Tequila => "Login with Tequila"
      case _                => sys.error("Unknown provider. This should never happen.")
    }

    def loginButton(provider: Provider, loggingInWith: Option[Provider]) =
      <.a(
        ^.classSet1("btn btn-primary", provider.id -> true),
        ^.onClick  --> onLogin(provider),
        ^.role      := "button",
        ^.href      := s"/login/${provider.id}",
        ^.disabled  := loggingInWith.isDefined,
        if (loggingInWith == Some(provider)) "Logging in…" else providerIcon(provider)
      )

    def onClose: Callback =
      $.modState(_.copy(loggingInWith = None)) >>
      $.props.flatMap(_.onRequestHide)

    def onLogin(provider: Provider): Callback =
      $.modState(_.copy(loggingInWith = Some(provider)))

    def render(state: State) =
      Modal(onClose)(
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
            """Click the one of the buttons below to start the authentication process."""
          ),
          <.div(^.className := "auth-buttons container-fluid",
            <.div(^.className := "row",
              <.div(^.className := "col-md-6 center",
                loginButton(Provider.GitHub, state.loggingInWith)
              ),
              <.div(^.className := "col-md-6 center",
                loginButton(Provider.Tequila, state.loggingInWith)
              )
            )
          )
        ),
        <.div(^.className := "modal-footer",
          closeButton(state.loggingInWith.isDefined)
        )
      )
  }

  val component =
    ReactComponentB[Props]("LoginModal")
      .initialState(State())
      .renderBackend[Backend]
      .build

  def apply(user: Option[User], onRequestHide: Callback) =
    component(Props(user, onRequestHide))

}

