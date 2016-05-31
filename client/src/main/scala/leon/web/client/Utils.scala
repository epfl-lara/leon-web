package leon.web
package client

import scala.scalajs.js.Dynamic.{ global => g }
import org.scalajs.jquery.{JQueryStatic, JQueryEventObject, JQuery}
import scala.scalajs.js
import js.annotation._
import org.scalajs.dom
import org.scalajs.dom.{console, document}
import org.scalajs.dom.Element
import org.scalajs.dom.raw.Selection
import org.scalajs.dom.raw.HTMLDocument

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
  def value: String = js.native
  def selectionStart: js.UndefOr[Int] = js.native
  def focus(): Unit = js.native
  var selectedIndex: Int = js.native
}

@js.native
trait HTMLDocumentExtended extends HTMLDocument {
  def selection: js.UndefOr[Selection] = js.native
}

@js.native
trait SelectionExtended extends Selection {
  def createRange(): dom.raw.Range = js.native
}

@js.native
trait RangeExtended extends dom.raw.Range {
  def moveStart(a: String, b: Int): Unit = js.native
  def text: String = js.native
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
  @inline implicit def toExtendedDocument(document: HTMLDocument): HTMLDocumentExtended = document.asInstanceOf[HTMLDocumentExtended]
  @inline implicit def toExtendedSelection(selection: Selection): SelectionExtended = selection.asInstanceOf[SelectionExtended]
  @inline implicit def toExtendedRange(range: dom.raw.Range): RangeExtended = range.asInstanceOf[RangeExtended]
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

object SelectionHandler {
  import dom.raw.Node
  
  private def getAbsCursorPosition(fromElem: Node, anchorNode: js.UndefOr[Node], acc: Int): Int = {
    if(anchorNode.isEmpty || anchorNode.get == null) {
      acc
    } else if(anchorNode.get == fromElem) {
      acc
    } else {
      // fromElem is an ancestor of anchorNode
      val parent = anchorNode.get.parentNode
      if(parent == null) acc else {
        val delta: Int = (0 until parent.childNodes.length).toList.map(
            parent.childNodes.apply(_)).takeWhile { p => p != anchorNode }.map(x => if(x.textContent == null) 0 else x.textContent.length).sum
        getAbsCursorPosition(fromElem, parent, acc + delta)
      }
    }
  }
  
  /** Retrieves the current selection */
  def getSelection(elem: Element): (Int, Int) = {
    val selection = g.window.getSelection().asInstanceOf[Selection]
    val a = getAbsCursorPosition(elem, selection.anchorNode, selection.anchorOffset)
    val b = if(js.isUndefined(selection.focusNode)) {
     a 
    } else getAbsCursorPosition(elem, selection.focusNode, selection.focusOffset)
    (a, b)
  }
  
  /** Gets the inner most element along with the corresponding position */
  private def getElementOffset(node: Node, pos: Int): js.UndefOr[(Node, Int)] = {
    if(node.nodeType == 3) {
      if(node.textContent != null && pos <= node.textContent.length) {
        (node, pos)
      } else {
        js.undefined
      }
    } else {
      var i = 0
      var ppos = pos
      while(i < node.childNodes.length) {
        val child = node.childNodes(i)
        if(child.textContent != null && child.textContent.length <= ppos) {
          var res = getElementOffset(child, ppos)
          if(res.isDefined) return res
        }
        ppos -= child.textContent.length
        i += 1
      }
      js.undefined
    }
  }
  
  /** Sets the selection of a particular range */
  def setSelection(elem: Element, startend: (Int, Int)): Unit = {
    setSelection(elem, startend._1, startend._2)
  }
  
  /** Sets the selection of a particular range.*/
  def setSelection(elem: Element, start: Int, end: Int): Unit = {
    val range = document.createRange();
    val s = getElementOffset(elem, start)
    val e = getElementOffset(elem, end)
    if(s.isDefined && e.isDefined) {
      val selection = g.window.getSelection().asInstanceOf[Selection]
      val (sn, sp) = s.get
      val (en, ep) = e.get
      range.setStart(sn, sp)
      range.setEnd(en, ep)
      selection.removeAllRanges();
      selection.addRange(range);
    } else {
      console.log(s"Could not set the position to $start, $end in")
      console.log(elem)
    }
  }
}