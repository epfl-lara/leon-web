/* Copyright 2009-2016 EPFL, Lausanne */

package leon.web
package client
package react
package components
package modals

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import leon.web.client.react.attrs._
import leon.web.client.data.{User, Identity}
import leon.web.shared.Provider

object AccountModal {

  case class Props(user: User, onRequestHide: Callback)
  case class State()

  class Backend($: BackendScope[Props, State]) {

    def closeButton(disabled: Boolean = false) =
      <.button(
        ^.className := "btn",
        ^.onClick  --> onClose,
        ^.disabled  := disabled,
        dataDismiss := "modal",
        "Close"
      )

    def onClose: Callback =
      $.props.flatMap(_.onRequestHide)

    def onClickUnlink(identity: Identity): Callback =
      Actions dispatchCB UnlinkAccount(identity.provider)

    def providerIcon(provider: Provider) = provider match {
      case Provider.GitHub  => "Link with GitHub"
      case Provider.Tequila => "Link with Tequila"
      case _                => sys.error("Unknown provider. This should never happen.")
    }

    def linkAccountButton(provider: Provider, disabled: Boolean = false) =
      <.a(
        ^.className := "btn btn-primary",
        ^.role      := "button",
        ^.href      := s"/login/${provider.id}",
        ^.disabled  := disabled,
        providerIcon(provider)
      )

    def renderIdentity(id: Identity, isMain: Boolean) =
      <.tr(^.key := id.provider.id, ^.classSet("active" -> isMain),
        <.td(^.className := "identity-provider", id.provider.id),
        <.td(^.className := "identity-email",    "(" + id.email + ")"),
        <.td(^.className := "identity-unlink text-right",
          !isMain ?= <.a(
            ^.className := "btn btn-danger btn-xs",
            ^.onClick --> onClickUnlink(id),
            "Unlink"
          )
        )
      )

    def render(props: Props, state: State) = {
      val user = props.user
      val ids  = user.identities.values.toSeq

      Modal(onClose)(
        <.div(^.className := "modal-header",
          Modal.closeButton(onClose),
          <.h3("Your account")
        ),
        <.div(^.className := "modal-body",
          <.div(^.className := "account-identities",
            <.h4("You are currently logged-in with the following providers:"),
            <.table(^.className := "table", <.tbody(
              ids.map(id => renderIdentity(id, id === user.main))
            ))
          ),
          <.div(^.className := "account-link-another",
            <.h4("Link another account:"),
            Provider.all.toSeq.map(p =>
              linkAccountButton(p, ids.map(_.provider) contains p)
            )
          )
        ),
        <.div(^.className := "modal-footer",
          closeButton()
        )
      )
    }
  }

  val component =
    ReactComponentB[Props]("AccountModal")
      .initialState(State())
      .renderBackend[Backend]
      .build

  def apply(user: User, onRequestHide: Callback) =
    component(Props(user, onRequestHide))

}

