package leon.web
package client

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.JSON
import org.scalajs.dom.ext.LocalStorage
import org.scalajs.jquery
import jquery.{ jQuery => $, JQueryAjaxSettings, JQueryXHR, JQuery, JQueryEventObject }
import org.scalajs.dom.html.Element

trait UndoRedoHandler { self: Main.type =>
  
  $("#button-undo").click(((self: Element, event: JQueryEventObject) => {
    if (!$(self).hasClass("disabled")) {
      doUndo()
    }
    event.preventDefault()
  }): js.ThisFunction);

  $("#button-redo").click(((self: Element, event: JQueryEventObject) => {
    if (!$(self).hasClass("disabled")) {
      doRedo()
    }
    event.preventDefault()
  }): js.ThisFunction);

  
  // Undo/Redo
  var backwardChanges = fromStorage[js.Array[String]]("backwardChanges").getOrElse(new js.Array[String])
  var forwardChanges  = fromStorage[js.Array[String]]("forwardChanges").getOrElse(new js.Array[String])

  def doUndo(): Unit = {
    forwardChanges.push(editor.getValue());
    val code = backwardChanges.pop();
    editor.setValue(code)
    editor.selection.clearSelection();
    editor.gotoLine(0);
    recompile();
    updateUndoRedo()
  }

  def doRedo(): Unit = {
    backwardChanges.push(editor.getValue());
    val code = forwardChanges.pop();
    editor.setValue(code)
    editor.selection.clearSelection();
    editor.gotoLine(0);
    recompile();
    updateUndoRedo()
  }

  

  def updateUndoRedo(): Unit = {
    val ub = $("#button-undo")
    val rb = $("#button-redo")

    if (backwardChanges.length > 0) {
      ub.removeClass("disabled")
    } else {
      ub.addClass("disabled")
    }

    if (forwardChanges.length > 0) {
      rb.removeClass("disabled")
    } else {
      rb.addClass("disabled")
    }

    if (backwardChanges.length > maxHistory) {
      backwardChanges.splice(0, backwardChanges.length - maxHistory)
    }

    if (forwardChanges.length > maxHistory) {
      forwardChanges.splice(0, forwardChanges.length - maxHistory)
    }

    LocalStorage.update("backwardChanges", JSON.stringify(backwardChanges))
    LocalStorage.update("forwardChanges", JSON.stringify(forwardChanges))
  }

  updateUndoRedo()
}