package main.scala.leon.web.client.websitebuilder

import com.scalawarrior.scalajs.ace._
import leon.web.client.MainDelayed
import leon.web.shared.StringPositionInSourceCode

import scala.scalajs.js
import scala.scalajs.js.Dynamic._
import js.Dynamic.{global => g, literal => l, newInstance => jsnew}

/**
  * Created by dupriez on 27/06/16.
  */
object EditorManipulator {
  val editor: Editor = MainDelayed.editor
  var previousMarkers = List[Int]()

  def updateEditor(
                    newValue: String,
                    triggerOnChangeCallback: Boolean = true,
                    positionsOfModificationInSourceCode: List[StringPositionInSourceCode]=List()): Unit = {
    val line = editor.session.getScrollTop()
    val col = editor.session.getScrollLeft()
    if(!triggerOnChangeCallback){
      leon.web.client.Main.unsetOnChangeCallbackForEditor()
    }
    editor.setValue(newValue)
    editor.selection.clearSelection()
    editor.session.setScrollTop(line)
    if(!triggerOnChangeCallback){
      leon.web.client.Main.setOnChangeCallbackForEditor()
    }
    updateMarkings(positionsOfModificationInSourceCode)
    positionsOfModificationInSourceCode.headOption.foreach(scrollToPosition)
  }


  def updateMarkings(positions: List[StringPositionInSourceCode]) = {
    clearMarkers(previousMarkers)
    previousMarkers = for(s <- positions) yield {
      s match {
        case StringPositionInSourceCode(lineFrom, colFrom, lineTo, colTo) =>
          addMarking(lineFrom, colFrom, lineTo, colTo)(3000)
      }
    }
  }

  private def clearMarkers(markersToClear: List[Int]) = {
    for(marker <- markersToClear) {
      editor.getSession().removeMarker(marker)
    }
  }

  def scrollToPosition(stringPositionInSourceCode: StringPositionInSourceCode) = {
    editor.getSession().setScrollTop(stringPositionInSourceCode.lineFrom - 1)
  }

  lazy val aceRange = ace.require("ace/range").Range
  /** Returns the ID of the added marker */
  private def addMarking(lineFrom: Int, colFrom: Int, lineTo: Int, colTo: Int)(timeoutms: Int): Int = {
      println(s"Adding marking at $lineFrom, $colFrom, $lineTo, $colTo of class tmp-highlight")
      val range = jsnew(aceRange)(lineFrom - 1, colFrom-1, lineTo - 1, colTo).asInstanceOf[Range]
      val marker = editor.getSession().addMarker(range, "tmp-highlight", "text", false)
      js.timers.setTimeout(timeoutms){
        editor.getSession().removeMarker(marker)
      }
      marker
  }
}
