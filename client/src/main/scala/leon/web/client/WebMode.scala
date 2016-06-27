package leon.web
package client

import org.scalajs.jquery.{jQuery => $}
import JQueryExtended.toJQueryExtended
import japgolly.scalajs.react.ReactDOM
import japgolly.scalajs.react.vdom.prefix_<^._
import leon.web.client.websitebuilder.Offset
import main.scala.leon.web.client.websitebuilder.ClarificationBox
import org.scalajs.dom.document


object WebMode {
  val LeonTitle = "Leon"
  val WebModeTitle = "WebBuilder"
  
  def webmodebutton  = $("#button-web")

  
  var codeColumnOldClass = ""
  val codecolumnNewClass = "col-lg-4 col-sm-4 col-xs-12"
  def activate() = {
    webmodebutton.removeClass("off")
    $(".panel > h3, #title, #overview .progress-bar, #invariant .progress-bar").addClass("webdesign", 800)
    val offset = $("#panelscolumn").offset().asInstanceOf[Offset]
    
    $("#panelscolumn").css("position", "absolute").css("left", offset.left+"px")
    $("#leon #allcontent #title h1").text(WebModeTitle)
    codeColumnOldClass = $("#codecolumn").attr("class").getOrElse("")
    $("#codecolumn").switchClass(codeColumnOldClass, codecolumnNewClass, 800)
    //$("#htmlDisplayerDiv").addClass("webdesigninitial")
    $("#htmlDisplayerDiv").show()
    scalajs.js.timers.setTimeout(800) {
      $("#htmlDisplayerDiv").removeClass("webdesigninitial", 400)
      ReactDOM.render(ClarificationBox.initialState, document.getElementById(WebBuildingUIManager.clarificationBoxID))
      Main.resizeEditor()
    }
  }
  def deactivate() = {
    webmodebutton.addClass("off")
    $(".panel > h3, #title, #overview .progress-bar, #invariant .progress-bar").removeClass("webdesign", 800)
    $("#codecolumn").switchClass(codecolumnNewClass, codeColumnOldClass, 800)
    $("#leon #allcontent #title h1").text(LeonTitle)
    //$("#htmlDisplayerDiv").addClass("webdesigninitial", 400)
    $("#htmlDisplayerDiv").hide()
    scalajs.js.timers.setTimeout(800) {
      Main.resizeEditor()
      $("#panelscolumn").css("position", "").css("left", "")
    }
  }
}