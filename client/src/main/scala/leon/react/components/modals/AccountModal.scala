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

    def renderIdentity(id: Identity, canUnlink: Boolean) =
      <.tr(^.key := id.provider.id,
        <.td(^.className := "identity-provider", id.provider.id),
        <.td(^.className := "identity-email",    "(" + id.email + ")"),
        <.td(^.className := "identity-unlink text-right",
          canUnlink ?= <.a(
            ^.className := "btn btn-danger btn-xs",
            ^.onClick --> onClickUnlink(id),
            "Unlink"
          )
        )
      )

    def renderIdentities(identities: Seq[Identity]) =
      <.div(^.className := "account-identities",
        <.h4("You are currently logged-in with the following providers:"),
        <.table(^.className := "table",
          <.tbody(
            identities.map { id =>
              renderIdentity(id, identities.length >= 2)
            }
          )
        )
      )

    def renderLinkAnotherAccount(providers: Seq[Provider], identities: Seq[Identity]) =
      <.div(^.className := "account-link-another",
        <.h4("Link another account:"),
        <.div(^.className := "container-fluid text-center",
          providers map { p =>
            <.div(^.className := "col-sm-6",
              linkAccountButton(p, identities.map(_.provider) contains p)
            )
          }
        )
      )

    def render(props: Props, state: State) = {
      val identities = props.user.identities.toSeq
      val providers  = Provider.all.toSeq

      Modal(onClose)(
        <.div(^.className := "modal-header",
          Modal.closeButton(onClose),
          <.h3("Your account")
        ),
        <.div(^.className := "modal-body",
          renderIdentities(identities),
          renderLinkAnotherAccount(providers, identities)
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

