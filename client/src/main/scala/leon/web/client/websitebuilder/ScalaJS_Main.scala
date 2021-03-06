package leon.web
package client
package websitebuilder

import scala.language.implicitConversions
import scala.scalajs.js
import js.Dynamic.{global => g, literal => l, newInstance => jsnew}
import scala.scalajs.js.annotation.ScalaJSDefined
import org.scalajs.dom.{Element, document, console}
import org.scalajs.jquery.{jQuery => $}
import com.scalawarrior.scalajs.ace._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.ReactDOM
import japgolly.scalajs.react.ReactNode
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import leon.webDSL.webDescription._
import shared.{PotentialWebPagesList, StringModificationSubmissionResult, StringPositionInSourceCode, _}
import leon.collection.{List => LeonList}

import scala.scalajs.js.timers.SetTimeoutHandle
import scalacss.ScalaCssReact._
import scalacss.mutable.StyleSheetRegistry
import scalacss.Defaults._
import messages._

/** **/


@ScalaJSDefined
trait Offset extends js.Any {
  val top: Double
  val left: Double
}

object ScalaJS_Main {
  def window = g
//  An attribute that SHOULD NOT be used by the end user, whose purpose is to serve as id for the html elements of the web interface
  val reservedAttributeForImplicitWebProgrammingID = "data-reservedattributeforimplicitwebprogrammingid".reactAttr
  val reservedAttributeForImplicitWebProgrammingID_name = "data-reservedattributeforimplicitwebprogrammingid"

  import leon.web.client.Main.Server
  
  def main(): Unit = {
    val registry = new StyleSheetRegistry
    registry.register(GlobalStyles)
    registry.addToDocumentOnRegistration()
    // create stylesheet
    //GlobalStyles.addToDocument()

    //    println("method main of ScalaJSExample is running")
    //    dom.document.getElementById("scalajsShoutOut").textContent = SharedMessages.itWorks
    //    displaySourceCode()
    //    includeScriptInMainTemplate(script("console.info(\"hey\")"))
    fillSourceCodeDiv()
    fillViewerDiv()
    
    if(js.Dynamic.global.location.href.indexOf("admin=true").asInstanceOf[Int] == -1) {
      $("#htmlMenu").hide()
    }

    $(window).resize(AceEditor.resizeEditor _);
    AceEditor.resizeEditor()

    /*TODO: Intention: the client would make an automatic code submission of the bootstrap code right after being loaded.
      TODO: In practice, it seems to sends an empty string as sourcecode to the server
    submitButtonAction() */
  }

  def idOfLastSourceCodeModificationSent = Backend.main.requestId
  def idOfLastSourceCodeModificationSent_=(v: Int) = Backend.main.requestId = v
  def submitSourceCode_serverAnswerHandler(sourceCodeProcessingResult: SubmitSourceCodeResult) = {
    println("Server sent something in response to a code submission")
    sourceCodeProcessingResult match {
      case SubmitSourceCodeResult(SourceCodeSubmissionResult(Some(webPage), log), javascript, requestId) => {
        if(requestId == idOfLastSourceCodeModificationSent) {
          println(
            s"""
               |Received "Some(WebPage)" for id = $requestId
                  """.stripMargin)
          //            webPage.asInstanceOf[WebPageWithIDedWebElements].sons.foldLeft(0)((useless, webElem) => {println(webElem.weid); useless})
          //            dom.document.getElementById("sourceCodeSubmitButton").setAttribute("style", "background-color:none")
          SourceCodeSubmitButton.removeCustomBackground()
          renderWebPage(webPage)
        } else {
          println(s"Received answer $requestId while expecting answer $idOfLastSourceCodeModificationSent from the server. Waiting.")
        }
      }
      case SubmitSourceCodeResult(SourceCodeSubmissionResult(None, log), javascript, _) =>
        println("Received \"None\" while expecting \"Some(WebPage)\" from the server")
    }
  }


  private var idOfLastStringModificationSent = 0
  
  import leon.web.client.Main.Server
  
  def submitStringModification(stringModification: StringModification) = {
    println(
      s"""Send String Modification:
        |WebElementID: ${stringModification.webElementID}
        |ModifiedWebAttribute: ${stringModification.modifiedWebAttribute}
        |NewValue: ${stringModification.newValue}
      """.stripMargin)
    idOfLastStringModificationSent += 1
    Server ! SubmitStringModification(stringModification, idOfLastSourceCodeModificationSent, idOfLastStringModificationSent)
  }
    
  Handlers += {
    case SubmitStringModification_answer(stringModificationSubmissionResult, requestSourceId, requestStringModID) => {
      println(
        s"""Received a SubmitStringModification_answer from the server containing:
            |  - StringModificationSubmissionResult
            |  - requestSourceID = $requestSourceId
            |  - requestStringModID = $requestStringModID
         """.stripMargin)
      stringModificationSubmissionResult match {
        case StringModificationSubmissionResult(potentialWebPageList, log) =>
          println("Here is the log attached to the received StringModificationSubmissionResult: " + "\n\t" + log)
          potentialWebPageList match {
            case PotentialWebPagesList(newSourceCodeIDOption, solutionList, idOfClarifiedWebElementOption) =>
              println("Received " + solutionList.length + " potential webPages")
              solutionList.length match {
                case 0 =>
                  println("[ERROR][UNDEFINED BEHAVIOUR]Received no potential webPage from the server")
                //                    TODO: Handle this case in some way
                case n =>
                  println("Received at least one potential webPage, checking ID conditions")
                  if (requestStringModID != idOfLastStringModificationSent || requestSourceId != idOfLastSourceCodeModificationSent) {
                    println("ID conditions not met, ignoring server message")
                  }
                  else {
                    println("ID conditions met. Rendering the first potential webPage and displaying its sourceCode")
                    idOfLastSourceCodeModificationSent = newSourceCodeIDOption.get
                    def setSourceCode(newSourceCode: String, changedElements: List[StringPositionInSourceCode]) = {
//                        AceEditor.removeAceEdOnChangeCallback()
                      AceEditor.setEditorValue(newSourceCode, triggerOnChangeCallback = false)
                      AceEditor.addMarkings(changedElements)
//                        AceEditor.activateAceEdOnChangeCallback_standard()
                    }
                    val firstSolution = potentialWebPageList.solutionList.head
                    renderWebPage(firstSolution.idedWebPage)
                    loadJavascript(lastJavascript)
                    
                    println("Displaying source code: "+firstSolution.sourceCode)
                    setSourceCode(firstSolution.sourceCode, firstSolution.positionsOfModificationsInSourceCode)
                    if (n > 1 && idOfClarifiedWebElementOption.isDefined) {
                      println("Received more than one (" + n + ") potential webPage from the server. Launching a clarification procedure")
                      println("idOfClarifiedWebElementOption= " + idOfClarifiedWebElementOption)
                      potentialWebPageList.solutionList.foldLeft(1)(
                        (index, solution) => {
                          println("Solution " + index + " :")
                          println("WebPage: " + solution.idedWebPage)
                          println("SourceCode: " + solution.sourceCode)
                          (index + 1)
                        }
                      )
                      //                        TODO: Launch clarification sequence
                      ClarificationBox.setSolutionButtons(potentialWebPageList.solutionList, idOfClarifiedWebElementOption.get)
                    }
                  }
              }
          }
      }
    }
  }
  
  def pointSourceOrigin(webId: Int, charIndex: Int) = {
    Server ! PointSourceProducingElement(webId, charIndex, idOfLastSourceCodeModificationSent)
  }
  Handlers += {
    case SourcePositionProducingElement(webId, sourceId, pos: StringPositionInSourceCode) =>
      AceEditor.addMarkings(List(pos))
  }

  def getElementByImplicitWebProgrammingID(impWebProgID: String) : org.scalajs.jquery.JQuery = {
//    $("["+reservedAttributeForImplicitWebProgrammingID_name+"="+impWebProgID+"]")
    println("getElementByImplicitWebProgrammingID, on ID: "+impWebProgID)
    println("jquery request: "+"["+reservedAttributeForImplicitWebProgrammingID_name+"="+impWebProgID+"]")
//    val res = dom.document.querySelector("["+reservedAttributeForImplicitWebProgrammingID_name+"="+impWebProgID+"]")
    val res = $("["+reservedAttributeForImplicitWebProgrammingID_name+"="+impWebProgID+"]")
//    $("[data-reservedattributeforimplicitwebprogrammingid="+impWebProgID+"]")
    println("end of getElementByImplicitWebProgrammingID")
    res
  }

  object SourceCodeSubmitButton {
    private val idOfThis = "sourceCodeSubmitButton"
    val scalaJSButton = <.button(
      reservedAttributeForImplicitWebProgrammingID := idOfThis,
      ^.className := "btn btn-default",
      ^.verticalAlign := "-webkit-baseline-middle",
      //^.onClick --> Callback{submitSourceCode()},
      "Run code"
    )
    private def getTheJSObject = {
//      dom.document.querySelector("["+reservedAttributeForImplicitWebProgrammingID_name+"="+idOfThis+"]")
      $("["+reservedAttributeForImplicitWebProgrammingID_name+"="+idOfThis+"]")
    }
    def turnBackgroundRed() = {
//      getTheJSObject.setAttribute("style", "background-color:red")
      getTheJSObject.removeClass("btn-default")
      getTheJSObject.addClass("btn-primary")
    }
    def removeCustomBackground() = {
//      getTheJSObject.setAttribute("style", "background-color:none")
      getTheJSObject.removeClass("btn-primary")
      getTheJSObject.addClass("btn-default")
    }
  }

//  The following is to allow the
  @ScalaJSDefined
  trait AugmentedElement extends Element {
    val value: js.UndefOr[String]
    val innerText: js.UndefOr[String]
    val text: js.UndefOr[String]
//    val textContent: js.UndefOr[String]
//    val textContent: String
  }
  implicit def elementAugmentation(e: Element): AugmentedElement ={
    e.asInstanceOf[AugmentedElement]
  }

  def fillSourceCodeDiv() = {
    val destinationDivId = "SourceCodeDiv"

    def submitHtmlButtonCallback = Callback{
        println("submit html change")
    }
    val submitHtmlButton = <.button(
      ^.className := "btn btn-secondary",
      ^.onClick --> submitHtmlButtonCallback,
      "Submit html change"
    )
    def stringModificationForm() = {
      val idField = <.input(
        reservedAttributeForImplicitWebProgrammingID := "idField_stringModificationForm",
        ^.`type` := "text", ^.name := "webEID", ^.placeholder := "1")
      val attributeField = <.input(
        reservedAttributeForImplicitWebProgrammingID := "attributeField_stringModificationForm",
        ^.`type` := "checkbox", ^.name := "attribute", ^.value := "Text", ^.placeholder := "Text")
      val attributeFieldLabel = "Text"
      val newValueField = <.input(
        reservedAttributeForImplicitWebProgrammingID := "newValueField_stringModificationForm",
        ^.`type` := "text", ^.name := "newValue", ^.placeholder := "newValue")

      /*def webAttributeNameToStringWebAttribute(waName: String) : Option[WebAttribute]= {
        waName match {
          case "Text" => Some(Text)
          case _ =>
            println("attribute name (\""+waName+"\")not recognised as a StringWebAttribute in function webAttributeNameToWebAttribute")
            None
        }
      }*/
      def submitButtonCallback = Callback{
          println("Submit string modification button clicked")
          var abort = false
//          val weID : Int = ("0" + dom.document.getElementById("idField_stringModificationForm").value.getOrElse("")).toInt
          val weID : Int = ("0" + getElementByImplicitWebProgrammingID("idField_stringModificationForm").value.getOrElse("")).toInt
//          println("weID=" + weID)
//          val webAttributeName = dom.document.getElementById("attributeField_stringModificationForm").getAttribute("value")
          val webAttributeName: String = getElementByImplicitWebProgrammingID("attributeField_stringModificationForm").attr("value").getOrElse("")
          val webAttribute =
            if (webAttributeName == "Text") None
            else Some(webAttributeName)
//          val newValue : String = dom.document.getElementById("newValueField_stringModificationForm").value.getOrElse("")
          val newValue : String = getElementByImplicitWebProgrammingID("newValueField_stringModificationForm").text()
//        println("newValue= "+ newValue)
          if(!abort) submitStringModification(StringModification(weID, webAttribute, newValue))

      }

      val submitStringModificationButton =
        <.button(
            "Submit String Modification",
            ^.className := "btn btn-primary",
            ^.onClick --> submitButtonCallback)

      <.div(
        idField,
        attributeField,
        attributeFieldLabel,
        newValueField,
        submitStringModificationButton
      )
    }
    val htmlMenu = <.div(
      ^.id := "htmlMenu",
      stringModificationForm(),
      submitHtmlButton
    )
    import scalacss.ScalaCssReact._
    val clarificationBox = ClarificationBox.initialState
    val divContent = <.div(
      ^.height := "100%",
      <.div("<< minimize", ^.id := "minimizeButton"),
      <.h1("Behind the scenes", ^.display.inline, ^.verticalAlign.middle),
      SourceCodeSubmitButton.scalaJSButton,
      htmlMenu,
      clarificationBox,
      <.div(^.id := "aceeditor")
    )
    ReactDOM.render(divContent, document.getElementById(destinationDivId))
    $("#minimizeButton").on("click", () => {
      minimizeSourceCodeView()
    })
  }

  def minimizeSourceCodeView(): Unit = {
    $("#SourceCodeDiv").animate(l(left = "-600px"), complete = () => {
      $("#ViewerDiv").removeClass("edited")
      $("#minimizeButton").text("Behind the scenes >>")
      val w = $("#minimizeButton").width()
      $("#minimizeButton").css("right", "-" + (w + 20) + "px").off("click").on("click", () => {
        $("#SourceCodeDiv").animate(l(left = "0px"), complete = () => {
          $("#ViewerDiv").addClass("edited")
          $("#minimizeButton").text("<< minimize").css("right", "10px").off("click").on("click", () => {
            minimizeSourceCodeView()
          })
        })
      })
    })
  }

  def fillViewerDiv() = {
    val destinationDivId = "ViewerDiv"
//    val f = stringModificationForm.

//    var i = 1
//    def testCallback = Callback{
//        println("testCallback")
//        val par = <.p(
//          "paragraph " + i
//        )
//        ReactDOM.render(par, document.getElementById("htmlDisplayerDiv"))
//        i=i+1
//        println("paragraph rendered")
//    }
//    val testButton = <.button(
//      ^.onClick --> testCallback,
//      "Test button"
//    )
    val htmlDisplayerDivContent = <.div(
      ^.id := WebBuildingUIManager.webPageDisplayerID
    )
    val divContent = <.div(
//      testButton,
      htmlDisplayerDivContent
    )
    ReactDOM.render(divContent, document.getElementById(destinationDivId))
  }

//  def fetchAndUseSourceCode(whatToDoWithSourceCode: String => Unit) = {
////    var sourceCode = ""
//    AjaxClient[Api].getSourceCode().call().onComplete {
//      case Failure(exception) => {println("Unable to fetch source code: " + exception.getMessage)}
//      case Success(fetchedsourceCode) => {println("ajax source code request success"); /*sourceCode = */whatToDoWithSourceCode(fetchedsourceCode)}
//    }
////    println(sourceCode)
////    println("sourceCode should have been printed (in function getSourceCode of ScalaJS_Main)")
////    sourceCode
//  }

  object AceEditor {
    //ID of the html div that should contain the aceeditor
    //val aceEditorID = "aceeditor"
    //Contains the aceEditor created
    //var aceEditor: Option[Editor] = None
    def aceEditor = Option(MainDelayed.editor)
    lazy val aceRange = ace.require("ace/range").Range;

    def getEditorValue = {
      aceEditor match {
        case Some(e) => e.getValue()
        case None => "[ERROR] fun getEditorValue was called while there was no aceEditor"
      }
    }

    def setEditorValue(value: String, triggerOnChangeCallback: Boolean = true) = {
      aceEditor match {
        case Some(e) =>
//          println("Setting Ace Editor value to: " + value)
          val line = e.session.getScrollTop()
          val col = e.session.getScrollLeft()
          if(!triggerOnChangeCallback){
            leon.web.client.Main.unsetOnChangeCallbackForEditor()
          }
          e.setValue(value)
          e.selection.clearSelection()
          e.session.setScrollTop(line)
          if(!triggerOnChangeCallback){
            leon.web.client.Main.setOnChangeCallbackForEditor()
          }
          //e.session.setScrollLeft(col) // Uncomment if the API works.
        case None => "[ERROR] fun setEditorValue was called while there was no aceEditor"
      }
    }



    var prevMarker = List[Int]()
    
    def addMarkings(positions: List[StringPositionInSourceCode]) = {
      for(marker <- prevMarker) {
        aceEditor.foreach(_.getSession().removeMarker(marker))
      }
      prevMarker = for(s <- positions) yield {
        s match {
          case StringPositionInSourceCode(lineFrom, colFrom, lineTo, colTo) =>
            AceEditor.addMarking(lineFrom, colFrom, lineTo, colTo)(3000)
        }
      }
      positions.headOption match {
        case Some(p) =>
          println("Scrolling to line " + p.lineFrom)
          aceEditor.foreach{editor =>
            editor.resize(true);
            editor.scrollToLine(p.lineFrom - 1, true, true, () => ())}
        case None =>
      }
    }
    
    /** Returns the ID of the added marker */
    def addMarking(lineFrom: Int, colFrom: Int, lineTo: Int, colTo: Int)(timeoutms: Int): Int = {
      aceEditor match {
        case Some(editor) =>
          println(s"Adding marking at $lineFrom, $colFrom, $lineTo, $colTo of class tmp-highlight")
          val range = jsnew(aceRange)(lineFrom - 1, colFrom-1, lineTo - 1, colTo).asInstanceOf[Range]
          val marker = editor.getSession().addMarker(range, "tmp-highlight", "text", false)
          js.timers.setTimeout(timeoutms){
              editor.getSession().removeMarker(marker)
          }
          marker
        case None => println("[ERROR] fun addMarking was called while there was no aceEditor")
          0
      }
    }

    def removeAceEdOnChangeCallback() = {
      currentOnChangeCallback = DoNothing_OnChangeCallback.onChangeCallback
//      currentOnChangeCallback = DoNothing_OnChangeCallback
    }

    def activateAceEdOnChangeCallback_standard() = {
      currentOnChangeCallback = Standard_OnChangeCallback.onChangeCallback
//      currentOnChangeCallback = Standard_OnChangeCallback
    }

    def resizeEditor(): Unit = {
      $("#SourceCodeDiv #aceeditor").css("height",
          "calc(100% - "+$("#aceeditor").offset().asInstanceOf[Offset].top+"px)")

      aceEditor.foreach(_.resize())
    };


//    @JSExport
//    def setSourceCodeRequestWillBeRemoved() = {
//      AjaxClient[Api].setSourceCode(getEditorValue).call().onComplete{
//        case Failure(exception) => {println("setSourceCode request failed: " + exception.getMessage)}
//        case Success(unit) => {println("setSourceCode request successful")}
//      }
//    }

    private trait OnChangeCallback {val onChangeCallback : js.Function1[scala.scalajs.js.Any, Unit]}

    private var currentOnChangeCallback/*: OnChangeCallback */= Standard_OnChangeCallback.onChangeCallback
    private val aceEdOnChangeCallbackVal_master/*: js.Function1[scala.scalajs.js.Any, Unit]*/ = aceEdOnChangeCallback_master _
    private def aceEdOnChangeCallback_master(uselessThingJustThereForTypingWithJavaScriptFunctions: scala.scalajs.js.Any) : Unit = {
      currentOnChangeCallback("useless")
    }

    private case object Standard_OnChangeCallback extends OnChangeCallback {override val onChangeCallback: js.Function1[scala.scalajs.js.Any, Unit] = aceEdOnChangeCallback_standard _}
    private def aceEdOnChangeCallback_standard(uselessThingJustThereForTypingWithJavaScriptFunctions: scala.scalajs.js.Any) : Unit= {
      SourceCodeSubmitButton.turnBackgroundRed()
    }

    private case object DoNothing_OnChangeCallback extends OnChangeCallback {override val onChangeCallback: js.Function1[scala.scalajs.js.Any, Unit] = aceEdOnChangeCallback_doNothing _}
    private def aceEdOnChangeCallback_doNothing(uselessThingJustThereForTypingWithJavaScriptFunctions: scala.scalajs.js.Any) : Unit = {
  }
  }

  object StringModificationSubmitter {
//  After the edition of a string by the user, the client will wait 'delay' ms before sending the StringModification to the server
//  The delay will be reinitialised after each modification To THE SAME ATTRIBUTE OF THE SAME ELEMENT!
//  (so modifications to other webElements/webAttributes done during the delay WILL NOT BE TAKEN INTO ACCOUNT)
    private val delay = 1000
    private var lastModification : StringModification = null
    private var timeout: Option[SetTimeoutHandle] = None
    private def stopTimeout() = {println("stop Text modification timeout"); timeout.foreach(to => js.timers.clearTimeout(to))}
    private def launchTimeout(stringModification: StringModification) = {
      println("launch Text modification timeout")
      timeout = Some(
          js.timers.setTimeout(delay){
            lastModification=null
            submitStringModification(stringModification)
          })
    }
    private def buildStringModification(webElementID: Int, newValue: String): StringModification = {
//      println("innerText of webElem with ID 7: "+getElementByImplicitWebProgrammingID("7")(0).innerText)
      //val newValue = getElementByImplicitWebProgrammingID(webElementID.toString)(0).innerText.getOrElse("getOrElseFailed in StringModificationSubmitter")
      StringModification(webElementID, None, newValue)
    }
    def newStringModificationOfTheTextWebAttr(webElementID: Int, newValue: String) = {
      if (lastModification != null) {
        if (lastModification.webElementID == webElementID) {
//          This new modification is on the same webElement and on the same WebAttribute than the current one.
          lastModification = buildStringModification(webElementID, newValue)
          stopTimeout()
          launchTimeout(lastModification)
        }
      }
      else {
        lastModification = buildStringModification(webElementID, newValue)
        launchTimeout(lastModification)
      }
    }
  }
  
  def renderCss(s: StyleSheet): String = {
    def renderStyle(s: WebStyle): String = {
      s.attributeName + ": " + s.attributeValue
    }
    def renderRule(r: StyleRule): String = {
      r.target.split(",").map("#"+WebBuildingUIManager.webPageDisplayerID+" " + _).mkString(",") +
        " {\n  " + LeonList.mkString(r.rules, ";\n  ", renderStyle) + "}\n\n"
    }
    LeonList.mkString(s.elems, "\n\n", renderRule)
  }

  var lastJavascript = "0"
  var lastRenderedWebpage: WebPageWithIDedWebElements = null
  def loadJavascript(s: String): Unit = {
    lastJavascript = s
    WebMode.evalJavascript(s)
  }
//  def includeScriptInMainTemplate(scriptTagToInclude: scalatags.JsDom.TypedTag[org.scalajs.dom.html.Script]) = {
//    dom.document.getElementById("scalajsScriptInclusionPoint").appendChild(scriptTagToInclude.render)
//  }
  //document.getElementById(htmlWebPageDisplayerDivID)
  def renderWebPage(webPageWithIDedWebElements: WebPageWithIDedWebElements/*, inElement: Element*/) = {
    println("Rendering " + webPageWithIDedWebElements)
    lastRenderedWebpage = webPageWithIDedWebElements
    val css = renderCss(webPageWithIDedWebElements.css) // TODO: Have this in the code natively so that we can modify it afterwards.
    
    val webPageDiv = <.div(
      ^.id := "webPage",
      convertWebElementWithIDToReactElement(webPageWithIDedWebElements.main)
    )
    val inElement = WebMode.htmlDisplayerDiv.find("#"+WebBuildingUIManager.webPageDisplayerID).get(0)
    ReactDOM.render(webPageDiv, inElement)
    WebMode.webpagecss.text(css)
  }

  def leonListToList[T](leonList: leon.collection.List[T]): List[T] = {
    val listBuffer = leonList.foldLeft(scala.collection.mutable.ListBuffer[T]())((list, elem)=>list += elem)
    listBuffer.toList
  }

//  an attribute to store the webElement IDs attributed during the execution of the program
  val impWebProgIDAttr = "data-impwebprogid".reactAttr


  /**
    * This function should only be given WebElementWithID
    *
    * @param webElWithID
    * @return
    */
  def convertWebElementWithIDToReactElement(webElWithID: WebElement) : ReactNode = {
//    val webElID = /*idGenerator.generateID()*/ 0
    def generateTextChangeCallback(webElID: Int, newValue: String) = {
      Callback{
        StringModificationSubmitter.newStringModificationOfTheTextWebAttr(webElID, newValue)
      }
    }
    def generateOriginCallback(webElID: Int, charIndex: Int) = {
      Callback{
        org.scalajs.dom.console.log("Looking to highlight element with id: " + webElID)
        pointSourceOrigin(webElID, charIndex)
      }
    }

    def splitTextIntoReactNodeSeq(text: String): Seq[ReactNode] = {
      text.split("(?=\n)").flatMap( (element: String) =>
        if(element.startsWith("\n")) {
          List[ReactNode](
            <.br,
            element.substring(1) : ReactNode
          )
        }
        else if(element == "") {
         List[ReactNode]()
        }
        else {
          List[ReactNode](element: ReactNode)
        }
      )
    }

    webElWithID match {
      case WebElementWithID(webElem, webElID) =>
        webElem match {
          case TextElement(text) =>
            val textChangeCallback = (e: ReactEvent) => generateTextChangeCallback(webElID, e.target.textContent)
            val onContextMenuCallback = (webElID: Int) => (e: ReactEvent) => {
              val x = e.nativeEvent.asInstanceOf[js.Dynamic].clientX
              val y = e.nativeEvent.asInstanceOf[js.Dynamic].clientY
              val charIndex =               
              if (!js.isUndefined(document.asInstanceOf[js.Dynamic].caretPositionFromPoint)) {    // standard
                  val range = document.asInstanceOf[js.Dynamic].caretPositionFromPoint(x, y);
                  range.offset.asInstanceOf[Int];
              } else if (!js.isUndefined(document.asInstanceOf[js.Dynamic].caretRangeFromPoint)) {    // WebKit
                  val range = document.asInstanceOf[js.Dynamic].caretRangeFromPoint(x, y);
                  range.startOffset.asInstanceOf[Int];
              } else 0
              generateOriginCallback(webElID, charIndex)
            }
            <.span(splitTextIntoReactNodeSeq(text),
              reservedAttributeForImplicitWebProgrammingID := webElID,
              ^.contentEditable := "true",
              ^.onChange ==> textChangeCallback,
              ^.onInput ==> textChangeCallback,
              ^.onContextMenu ==> onContextMenuCallback(webElID)
              //,
              //^.title := "webElID= "+webElID
            )
          case Element(tag, sons, attributes, styles) =>
            tag.reactTag(
              reservedAttributeForImplicitWebProgrammingID := webElID,
              leonListToList(sons).map(convertWebElementWithIDToReactElement),
              //^.title := "webElID= "+webElID,
              leonListToList(attributes).map{ x =>
                if(x.attributeName == "class") {
                  ^.className := x.attributeValue 
                } else  x.attributeName.reactAttr := x.attributeValue
              },
              leonListToList(styles).map{ x => x.attributeName.reactStyle := x.attributeValue }
            )
          case WebElementWithID(_,_) =>
    //        Should never happen
            println("Erreur: convertWebElementToReactElement was given a WebElementWithID (good) wrapping a WebElementWithID (bad)")
            <.p()
        }
      case _ =>
//        Not supposed to happen, since convertWebElementToReactElement should only be given webElementWithID
        println(
          s"""Erreur: convertWebElementToReactElement was given something else than a WebElementWithID:
             |  argument: $webElWithID
           """.stripMargin)
        <.p()
    }
  }

}
