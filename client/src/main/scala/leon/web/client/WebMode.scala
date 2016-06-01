package leon.web
package client


import scala.language.reflectiveCalls
import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.JSON
import org.scalajs.dom
import org.scalajs.dom.{console, document}
import org.scalajs.dom.html.Element
import org.scalajs.dom.WebSocket
import org.scalajs.dom.{Event, MessageEvent, CloseEvent, ErrorEvent}
import org.scalajs.dom.ext.LocalStorage
import scala.scalajs.js.Dynamic.{ global => g, literal => l/*, newInstance => jsnew*/ }
import japgolly.scalajs.react._
import com.scalawarrior.scalajs.ace._
import org.scalajs.jquery
import jquery.{ jQuery => $, JQueryAjaxSettings, JQueryXHR, JQuery, JQueryEventObject }
import JQueryExtended._
import Bool._
import Implicits._
import shared.{VerifStatus, TerminationStatus, InvariantStatus, Constants}
import shared.{Module => ModuleDescription, Main => MainModule, _}
import shared.{Project}
import shared.equal.EqSyntax
import client.react.{App => ReactApp}
import client.react.Actions
import client.utils.BufferedWebSocket
import shared.messages.{Event => _, _}
import scala.scalajs.js.typedarray._
import java.nio.ByteBuffer
import org.scalajs.dom.raw.FileReader

object WebMode {
  def activate() = {
    $(".panel > h3, #title, #overview .progress-bar, #invariant .progress-bar").addClass("webdesign", 800)
    
    
  }
  def deactivate() = {
    $(".panel > h3, #title, #overview .progress-bar, #invariant .progress-bar").removeClass("webdesign", 800)
  }
}