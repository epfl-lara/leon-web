package leon.web
package client

import org.scalajs.jquery.{jQuery => $, _}
import JQueryExtended.toJQueryExtended
import japgolly.scalajs.react.ReactDOM
import japgolly.scalajs.react.vdom.prefix_<^._
import leon.web.client.websitebuilder.Offset
import org.scalajs.dom.document
import leon.web.client.websitebuilder.ScalaJS_Main
import scala.scalajs.js
import js.Dynamic.{global => g, newInstance => jsnew, literal => l}
import org.scalajs.dom.{window, _}
import js.annotation.ScalaJSDefined
import org.scalajs.dom.html.Element
import scalacss.Defaults._

object WebMode {
  
  val LeonTitle = "Leon"
  val WebModeTitle = "WebBuilder"
  
  def webmodebutton  = $("#button-web")

  
  var codeColumnOldClass = ""
  val codecolumnNewClass = "col-lg-4 col-sm-4 col-xs-12"
  
  var htmlDisplayerDivOldClass = ""
  var htmlDisplayerDivNewlass = ""
  
  lazy val htmlDisplayerDiv = $("#htmlDisplayerDiv")
  private
  lazy val leonTitle = $("#leon #allcontent #title h1")
  lazy val codecolumn = $("#codecolumn")
  lazy val panelscolumn = $("#panelscolumn")
  lazy val allThingsToPutInWebDesign = $(".panel > h3, #title, #overview .progress-bar, #invariant .progress-bar")
  
  lazy val htmlDisplayerDivElement = htmlDisplayerDiv.get(0)
  lazy val webpagecss: JQuery = {
    val res = $("#webpagecss")
    if(res.length == 0) {
      val wpcss = $("<style>").attr("id", "webpagecss")
      $("head").append(wpcss)
      wpcss
    } else {
      res
    }
  }
  
  var previewWindow: Window = null
  
  def body = if(previewWindow == null) $("body") else $(previewWindow.document).find("body")
  
  def isWebModeOn: Boolean = !webmodebutton.hasClass("off")
  def isWindowExternalized: Boolean = previewWindow != null
  
  def evalJavascript(s: String) = {
    if(isWindowExternalized) {
      previewWindow.asInstanceOf[js.Dynamic].eval(s)
    } else {
      g.eval(s)
    }
  }
  
  // Call this method only when the window is in the main window.
  private def setExternIconAction(externalize: Boolean): Unit = {
    $("#separatewindowlink").off("click.popup").on("click.popup", { () =>
      if(externalize) externalizeWebPanel() else internalizeWebPanel()
    })
  }
  
  private def smallilfyCodeColumn() = {
    val offset = panelscolumn.offset().asInstanceOf[Offset]
    panelscolumn.css("position", "absolute").css("left", offset.left+"px")
    codeColumnOldClass = codecolumn.attr("class").getOrElse("")
    codecolumn.switchClass(codeColumnOldClass, codecolumnNewClass, 800)
    scalajs.js.timers.setTimeout(800) {
      Main.resizeEditor()
    }
  }
  
  private def greatifyCodeColumn() = {
    codecolumn.switchClass(codecolumnNewClass, codeColumnOldClass, 800)
    scalajs.js.timers.setTimeout(800) {
      Main.resizeEditor()
      panelscolumn.css("position", "").css("left", "")
    }
  }
  
  private def externalizeWebPanel() = {
    setExternIconAction(externalize = false)
    previewWindow = window.open(window.location.origin.getOrElse("") + "/preview", "Preview", "width=400,height=400")
    previewWindow.addEventListener("load", ((e: Event) => {
      $(previewWindow.document).find("head").append(webpagecss).append($("<style>").text(GlobalStyles.render[String]))
      $(previewWindow.document).find("body").append(htmlDisplayerDiv) // Move htmlDisplayerDiv to the outer window.
      htmlDisplayerDiv.find("#"+WebBuildingUIManager.webPageDisplayerID).empty()
      ScalaJS_Main.renderWebPage(ScalaJS_Main.lastRenderedWebpage)
      
      htmlDisplayerDivOldClass = htmlDisplayerDiv.attr("class").getOrElse("")
      htmlDisplayerDiv.attr("class", htmlDisplayerDivNewlass)
      evalJavascript(ScalaJS_Main.lastJavascript)
      
      greatifyCodeColumn()
      ().asInstanceOf[js.Any]
    }), false)
  }
  
  private def internalizeWebPanel(executeJavascript: Boolean = true) = {
    smallilfyCodeColumn()
    $("head").append(webpagecss)
    codecolumn.after(htmlDisplayerDiv.attr("class", htmlDisplayerDivOldClass))
    previewWindow.close()
    previewWindow = null
    setExternIconAction(externalize = true)
    if(executeJavascript) evalJavascript(ScalaJS_Main.lastJavascript)
  }
  
  private def insertWebPanel() = {
    smallilfyCodeColumn()
    
    htmlDisplayerDiv.show()
    scalajs.js.timers.setTimeout(800) {
      htmlDisplayerDiv.removeClass("webdesigninitial", 400)
      ReactDOM.render(ClarificationBox.initialState, document.getElementById(WebBuildingUIManager.clarificationBoxID))
      Main.resizeEditor()
    }
    setExternIconAction(externalize = true)
  }
  
  private def removeWebPanel() = {
    htmlDisplayerDiv.hide()
    greatifyCodeColumn()
  }
  
  def activate() = {
    webmodebutton.removeClass("off")
    allThingsToPutInWebDesign.addClass("webdesign", 800)
    leonTitle.text(WebModeTitle)
    insertWebPanel()
    js.timers.setTimeout(800){
      Main.showContextDemo(Main.demoWebpageFullscreen)
    }
    if($("#downloadhtmlpage").length == 0) {
       $("#additionalclflags").after($("<input id='downloadhtmlpage' type='button' value='Download Html Page'>"))
       $("#downloadhtmlpage").click(() => {
         Main.Server ! shared.messages.DownloadWebpage()
       })
    }
    $("#downloadhtmlpage").show()
  }
  import Implicits._
  Handlers += { case shared.messages.DownloadWebpage_answer(filename, data) =>
      val blob = jsnew(g.Blob)(js.Array(data), l("type" -> "text/html"));
      if(!js.isUndefined(g.window.navigator.msSaveOrOpenBlob)) {
          g.window.navigator.asInstanceOf[Navigator].msSaveBlob(blob, filename);
      }
      else{
          var elem = window.document.createElement("a");
          elem.href = g.window.URL.asInstanceOf[AugmentedWindowURL].createObjectURL(blob);
          elem.download = filename;        
          document.body.appendChild(elem);
          elem.click();        
          document.body.removeChild(elem);
      }
  }
  
  def deactivate() = {
    webmodebutton.addClass("off")
    allThingsToPutInWebDesign.removeClass("webdesign", 800)
    leonTitle.text(LeonTitle)
    if(isWindowExternalized) {
      htmlDisplayerDiv.hide()
      $("head").append(webpagecss)
      codecolumn.after(htmlDisplayerDiv.attr("class", htmlDisplayerDivOldClass))
      previewWindow.close()
      previewWindow = null
      setExternIconAction(externalize = true)
    } else {
      removeWebPanel()
    }
    $("#downloadhtmlpage").hide()
  }
}