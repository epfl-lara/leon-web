package leon.web.client
package react

import scala.scalajs.js
import org.scalajs.dom.document
import org.scalajs.jquery
import jquery.{ jQuery => $, JQueryEventObject }
import JQueryExtended._

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

object App {

  import leon.web.client.components._
  import leon.web.client.components.modals._

  def render(): Unit = {
    renderLogin()
    renderLoadRepoPanel()
  }

  private def renderLogin(): Unit = {
    val el = document.getElementById("login-modal")
    ReactDOM.render(LoginModal(false), el)

    $("#login-btn").click { e: JQueryEventObject =>
      e.preventDefault()
      ReactDOM.render(LoginModal(true), el)
    }
  }

  private def renderLoadRepoPanel(): Unit = {
    val panelEl = document.getElementById("load-repo-panel")

    if (panelEl != null) {
      ReactDOM.render(LoadRepositoryPanel(), panelEl)
      $(panelEl).show()
    }
  }

}
