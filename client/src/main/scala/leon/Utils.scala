package leon.web
package client

import org.scalajs.jquery.{JQueryStatic, JQueryEventObject, JQuery}
import org.scalajs.dom.html.Element
import scala.scalajs.js
import js.annotation._

/**
 * Created by Mikael on 10.08.2015.
 */
@js.native
trait JQueryExtended extends JQuery {
  def width(value: String): JQuery = js.native
  def alert(): JQuery = js.native

  @JSName("val") def value(e: js.Any): JQuery = js.native
  def html(e: js.Any): JQuery = js.native

  def autocomplete(e: js.Any, f: js.Any = null): JQuery = js.native
  
  def tooltip(e: js.Any): JQuery = js.native
  
  def on(e: String, f: () => Unit): JQuery = js.native
  
  def modal(e: String): JQuery = js.native
  
  def popover(parameters: js.Any): JQuery = js.native
}

@js.native
trait HTMLElementExtended extends Element {
  def innerText: js.UndefOr[String] = js.native
}

@js.native
trait JQueryEventObjectExtended extends JQueryEventObject {
  val keyCode: Int = js.native
}

@js.native
trait JQueryStaticExtended extends js.Any {
  val ui: JQueryStaticExtendedUi = js.native
}
@js.native
trait JQueryStaticExtendedUi extends js.Any {
  val autocomplete: JQueryStaticExtendedUiAutoComplete = js.native
}
@js.native
trait JQueryStaticExtendedUiAutoComplete extends js.Any {
  def filter(a: js.Array[String], part: String): js.Array[String] = js.native
}


object JQueryExtended {
  implicit def toJQueryStaticExtended(t: JQueryStatic): JQueryStaticExtended = t.asInstanceOf[JQueryStaticExtended]
  @inline implicit def toJQueryExtended(t: JQuery): JQueryExtended = t.asInstanceOf[JQueryExtended]
  @inline implicit def dynamicToBoolean(d: js.Dynamic): Boolean = d.asInstanceOf[Boolean]
  //@inline implicit def dynamicToString(d: js.Dynamic): String = d.asInstanceOf[String]
  //@inline implicit def dynamicToHandlerDataArgument(d: js.Dynamic): HandlerDataArgument = d.asInstanceOf[HandlerDataArgument]

  @inline implicit def toJQueryEventObjectExtended(t: JQueryEventObject) = t.asInstanceOf[JQueryEventObjectExtended]

  /*implicit class ComparisonOp(d: js.Dynamic) {
    def ==(other: String) = d.asInstanceOf[String] == other
  }*/
}

object Implicits {
  implicit class NullCheck[T](s: T) {
    def orIfNull(default: =>T): T = if(s == null || js.isUndefined(s)) default else s
  }
  @inline implicit def toHTMLElementExtended(t: Element): HTMLElementExtended = t.asInstanceOf[HTMLElementExtended] 
}

case class Bool(b: Boolean) {   
  def ?[X](t: => X) = new {
    def |(f: => X) = if(b) t else f   
  } 
}

object Bool {   
  implicit def BooleanBool(b: Boolean) = Bool(b) 
}

@js.native
trait ObjectWithBracketAccess extends js.Object {
  @JSBracketAccess
  def apply(index: String): js.Dynamic = js.native
}
