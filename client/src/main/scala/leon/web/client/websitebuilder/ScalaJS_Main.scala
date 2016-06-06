package leon.web
package client
package websitebuilder

import scala.language.implicitConversions
import scala.scalajs.js
import js.Dynamic.{ global => g, literal => l, newInstance => jsnew }
import scala.scalajs.js.annotation.ScalaJSDefined
import org.scalajs.dom.Element
import org.scalajs.dom.document
import org.scalajs.jquery.{ jQuery => $ }
import com.scalawarrior.scalajs.ace._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.ReactDOM
import japgolly.scalajs.react.ReactNode
import japgolly.scalajs.react.vdom.prefix_<^._
import leon.webDSL.webDescription._
import leon.web.shared._
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

  //private var idOfLastSourceCodeModificationSent = 0
  def submitSourceCode() = { // Already dealt in handlers.scala
    /*idOfLastSourceCodeModificationSent += 1
    println(s"submit source code change with requestId = $idOfLastSourceCodeModificationSent")
    Server ![SubmitSourceCodeResult] (SubmitSourceCode(AceEditor.getEditorValue, idOfLastSourceCodeModificationSent), {
      case SubmitSourceCodeResult(SourceCodeSubmissionResult(Some(webPage), log), requestId) => {
        if(requestId == idOfLastSourceCodeModificationSent) {
          println(
            s"""
               |Received "Some(WebPage)" for id = $requestId
                  """.stripMargin)
          //            webPage.asInstanceOf[WebPageWithIDedWebElements].sons.foldLeft(0)((useless, webElem) => {println(webElem.weid); useless})
          //            dom.document.getElementById("sourceCodeSubmitButton").setAttribute("style", "background-color:none")
          SourceCodeSubmitButton.removeCustomBackground()
          renderWebPage(webPage, "htmlDisplayerDiv")
        } else {
          println(s"Received answer $requestId while expecting answer $idOfLastSourceCodeModificationSent from the server. Waiting.")
        }
      }
      case SubmitSourceCodeResult(SourceCodeSubmissionResult(None, log), _) =>
        println("Received \"None\" while expecting \"Some(WebPage)\" from the server")
    })*/

//    CallbackForServerMessages(SubmitSourceCode(SourceCodeSubmissionNetwork(AceEditor.getEditorValue, idOfLastSourceCodeModificationSent))){
//      case submitSourceCode_answer: SubmitSourceCode_answer => {
//        println("Server sent something in response to a code submission")
//        submitSourceCode_answer match {
//          case SourceCodeSubmissionResultNetwork(SourceCodeSubmissionResult(Some(webPage), log), requestId) => {
//            if(requestId == idOfLastSourceCodeModificationSent) {
//              println(
//                s"""
//                   |Received "Some(WebPage)" for id = $requestId
//                  """.stripMargin)
//              //            webPage.asInstanceOf[WebPageWithIDedWebElements].sons.foldLeft(0)((useless, webElem) => {println(webElem.weid); useless})
//              //            dom.document.getElementById("sourceCodeSubmitButton").setAttribute("style", "background-color:none")
//              SourceCodeSubmitButton.removeCustomBackground()
//              renderWebPage(webPage, "htmlDisplayerDiv")
//            } else {
//              println(s"Received answer $requestId while expecting answer $idOfLastSourceCodeModificationSent from the server. Waiting.")
//            }
//          }
//          case SourceCodeSubmissionResultNetwork(SourceCodeSubmissionResult(None, log), _) =>
//            println("Received \"None\" while expecting \"Some(WebPage)\" from the server")
//        }
//      }
//    }

//    AjaxClient[Api].request(SubmitSourceCode(SourceCodeSubmissionNetwork(AceEditor.getEditorValue, idOfLastSourceCodeModificationSent))).call().onComplete {
//      case Failure(exception) => {println("error during submission of the source code: " + exception)}
//      case Success(serverAnswer) => CallbackForServerMessages.callbackForServerMessages(serverAnswer)
//    }
//    #Original version, before the migration to leon web client-server communication model
//    AjaxClient[Api].submitSourceCode(SourceCodeSubmissionNetwork(AceEditor.getEditorValue, idOfLastSourceCodeModificationSent)).call().onComplete {
//      case Failure(exception) => {println("error during submission of the source code: " + exception)}
//      case Success(sourceCodeProcessingResult) => {
//        println("Server sent something in response to a code submission")
//        sourceCodeProcessingResult match {
//          case SourceCodeSubmissionResultNetwork(SourceCodeSubmissionResult(Some(webPage), log), requestId) => {
//            if(requestId == idOfLastSourceCodeModificationSent) {
//            println(
//              s"""
//                 |Received "Some(WebPage)" for id = $requestId
//                  """.stripMargin)
////            webPage.asInstanceOf[WebPageWithIDedWebElements].sons.foldLeft(0)((useless, webElem) => {println(webElem.weid); useless})
////            dom.document.getElementById("sourceCodeSubmitButton").setAttribute("style", "background-color:none")
//            SourceCodeSubmitButton.removeCustomBackground()
//            renderWebPage(webPage, "htmlDisplayerDiv")
//            } else {
//              println(s"Received answer $requestId while expecting answer $idOfLastSourceCodeModificationSent from the server. Waiting.")
//            }
//          }
//          case SourceCodeSubmissionResultNetwork(SourceCodeSubmissionResult(None, log), _) =>
//            println("Received \"None\" while expecting \"Some(WebPage)\" from the server")
//        }
//      }
//    }
  }
  def idOfLastSourceCodeModificationSent = Backend.main.requestId
  def idOfLastSourceCodeModificationSent_=(v: Int) = Backend.main.requestId = v
  def submitSourceCode_serverAnswerHandler(sourceCodeProcessingResult: SubmitSourceCodeResult) = {
    println("Server sent something in response to a code submission")
    sourceCodeProcessingResult match {
      case SubmitSourceCodeResult(SourceCodeSubmissionResult(Some(webPage), log), requestId) => {
        if(requestId == idOfLastSourceCodeModificationSent) {
          println(
            s"""
               |Received "Some(WebPage)" for id = $requestId
                  """.stripMargin)
          //            webPage.asInstanceOf[WebPageWithIDedWebElements].sons.foldLeft(0)((useless, webElem) => {println(webElem.weid); useless})
          //            dom.document.getElementById("sourceCodeSubmitButton").setAttribute("style", "background-color:none")
          SourceCodeSubmitButton.removeCustomBackground()
          renderWebPage(webPage, "htmlDisplayerDiv")
        } else {
          println(s"Received answer $requestId while expecting answer $idOfLastSourceCodeModificationSent from the server. Waiting.")
        }
      }
      case SubmitSourceCodeResult(SourceCodeSubmissionResult(None, log), _) =>
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
    Server ![SubmitStringModificationResult] (SubmitStringModification(stringModification, idOfLastSourceCodeModificationSent, idOfLastStringModificationSent), {
      case SubmitStringModificationResult(stringModificationSubmissionResult, sourceId, stringModID) => {
        println("Server sent something in response to a string modification submission")
        stringModificationSubmissionResult match {
          case StringModificationSubmissionResult(Some(StringModificationSubmissionConcResult(newSourceCode, positions, newId, webPageWithIDedWebElements)), log) => {
//            println(
//              s"""
//                 |Received new source code with stringModificationID of $stringModID: $newSourceCode
//                  """.stripMargin)
            println(
              s"""
                 |Received new source code with stringModificationID of $stringModID: TEMPORARY DISABLED
                  """.stripMargin)
            if (stringModID == idOfLastStringModificationSent && sourceId == idOfLastSourceCodeModificationSent ) {
              idOfLastSourceCodeModificationSent = newId
              renderWebPage(webPageWithIDedWebElements, "htmlDisplayerDiv")
              println("Accepting the stringModificationResult with id: "+stringModID)
            }
            else {
              println("Rejecting outdated stringModificationResult (id= "+stringModID+", while the id of the last StringModification sent is: "+idOfLastStringModificationSent+")")
            }
//            remove the standard onChange callback of the Ace Editor, so that the "submit source code change" button does not turn red
//            because of the following call to AceEditor.setEditorValue
            AceEditor.removeAceEdOnChangeCallback()
            AceEditor.setEditorValue(newSourceCode)
            AceEditor.addMarkings(positions)

            AceEditor.activateAceEdOnChangeCallback_standard()
          }
          case StringModificationSubmissionResult(None, log) => {
            println("Received \"None\" while expecting \"Some(newSourceCode)\" from the server")
            println("Received a StringModificationSubmissionResult from the server, but without sourceCode. Here is the log sent by the server:")
            println("\"" + log + "\"")
          }
        }
      }
    })
  }
  def submitStringModification_serverAnswerHandler(stringModificationSubmissionResultForNetwork: SubmitStringModificationResult) = {
    println("Server sent something in response to a string modification submission")
    stringModificationSubmissionResultForNetwork match {
      case SubmitStringModificationResult(
      StringModificationSubmissionResult(Some(StringModificationSubmissionConcResult(newSourceCode, positions, newId, webPageWithIDedWebElements)), log),
      sourceId,
      stringModID
      ) => {
        //            println(
        //              s"""
        //                 |Received new source code with stringModificationID of $stringModID: $newSourceCode
        //                  """.stripMargin)
        println(
          s"""
             |Received new source code with stringModificationID of $stringModID: TEMPORARY DISABLED
                  """.stripMargin)
        if (stringModID == idOfLastStringModificationSent && sourceId == idOfLastSourceCodeModificationSent ) {
          idOfLastSourceCodeModificationSent = newId
          renderWebPage(webPageWithIDedWebElements, "htmlDisplayerDiv")
          println("Accepting the stringModificationResult with id: "+stringModID)
        }
        else {
          println("Rejecting outdated stringModificationResult (id= "+stringModID+", while the id of the last StringModification sent is: "+idOfLastStringModificationSent+")")
        }
        //            remove the standard onChange callback of the Ace Editor, so that the "submit source code change" button does not turn red
        //            because of the following call to AceEditor.setEditorValue
        AceEditor.removeAceEdOnChangeCallback()
        AceEditor.setEditorValue(newSourceCode)
        AceEditor.addMarkings(positions)

        AceEditor.activateAceEdOnChangeCallback_standard()
      }
      case SubmitStringModificationResult(
      StringModificationSubmissionResult(None, log),
      sourceId,
      stringModID
      ) => {
        println("Received \"None\" while expecting \"Some(newSourceCode)\" from the server")
        println("Received a StringModificationSubmissionResult from the server, but without sourceCode. Here is the log sent by the server:")
        println("\"" + log + "\"")
      }
    }
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
      ^.onClick --> Callback{submitSourceCode()},
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
    val discussionBox = <.div(
      ^.id := "discussionbox",
      GlobalStyles.discussionbox,
      <.div(
          ^.id := "discussionCommentId",
          "Weeeeeeeelcome behind the scenes.",
          GlobalStyles.discussionComment,
          GlobalStyles.triangleBorderRight
      ),
      Clipart.goat()
    )

    val divContent = <.div(
      ^.height := "100%",
      <.div("<< minimize", ^.id := "minimizeButton"),
      <.h1("Behind the scenes", ^.display.inline, ^.verticalAlign.middle),
      SourceCodeSubmitButton.scalaJSButton,
      htmlMenu,
      discussionBox,
      <.div(^.id := "aceeditor")
    )
    ReactDOM.render(divContent, document.getElementById(destinationDivId))
    $("#minimizeButton").on("click", () => {
      minimizeSourceCodeView()
    })
    AceEditor.initialiseAndIncludeEditorInWebPage()
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
    val htmlDisplayerDiv = <.div(
      ^.id := "htmlDisplayerDiv"
    )
    val divContent = <.div(
//      testButton,
      htmlDisplayerDiv
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

    def initialiseAndIncludeEditorInWebPage() = {
      /*val editor = ace.edit(aceEditorID)
      aceEditor = Some(editor)
      //editor.setTheme("ace/theme/monokai")
      //ace.require("ace/token_tooltip"); // From leon-web
      editor.setTheme("ace/theme/chrome");
      editor.getSession().setUseWrapMode(true)
      editor.setShowPrintMargin(false);
      editor.setAutoScrollEditorIntoView();
      editor.setHighlightActiveLine(false);
      
      editor.getSession().setMode("ace/mode/scala")
      editor.getSession().setTabSize(2)
//      updateEditorContent()

      Server ![GetBootstrapSourceCode_answer] (GetBootstrapSourceCode(), {
        case GetBootstrapSourceCode_answer(Some(bootstrapSourceCode)) =>
          println("ajax bootstrap source code request success")
          setEditorValue(bootstrapSourceCode)
          editor.getSession().on("change", aceEdOnChangeCallbackVal_master)
        //              editor.getSession().on("change", DoNothing_OnChangeCallback.onChangeCallback)
          activateAceEdOnChangeCallback_standard()
          submitSourceCode()
        case GetBootstrapSourceCode_answer(None) =>
          println("Error: did not received bootStrapSourceCode in response to a GetBootstrapSourceCode message sent to the server")
      })
//      AjaxClient[Api].sendToServer(GetBootstrapSourceCode()).call().onComplete {
//        case Failure(exception) => {println("Unable to fetch bootstrap source code: " + exception.getMessage)}
//        case Success(serverAnswer) => CallbackForServerMessages.callbackForServerMessages(serverAnswer)
//        #Original version, before the migration to leon web client-server communication model
//          serverReturn match {
//            case Left(bootstrapSourceCode) =>
//              println("ajax bootstrap source code request success")
//              setEditorValue(bootstrapSourceCode)
//              editor.getSession().on("change", aceEdOnChangeCallbackVal_master)
////              editor.getSession().on("change", DoNothing_OnChangeCallback.onChangeCallback)
//              activateAceEdOnChangeCallback_standard()
//              submitSourceCode()
//            case Right(serverError) =>
//              println("ajax bootstrap source code request failed: It triggered the following server error: "+serverError.text)
//          }

      resizeEditor()


      //    val sourceCode = getSourceCode()
      //    println(sourceCode)
      //    println("sourceCode should have been printed (in function initialiseAndIncludeAceEditorForSourceCode of ScalaJS_Main)")
      //    editor.setValue(sourceCode)
       *    
       */
    }
//    def getBootstrapSourceCode_serverAnswerHandler(serverAnswer: Either[String, ServerError]) = {
//      serverAnswer match {
//        case Left(bootstrapSourceCode) =>
//          println("ajax bootstrap source code request success")
//          setEditorValue(bootstrapSourceCode)
//          aceEditor.get.getSession().on("change", aceEdOnChangeCallbackVal_master)
//          //              editor.getSession().on("change", DoNothing_OnChangeCallback.onChangeCallback)
//          activateAceEdOnChangeCallback_standard()
//          submitSourceCode()
//        case Right(serverError) =>
//          println("ajax bootstrap source code request failed: It triggered the following server error: "+serverError.text)
//      }
//    }


//    def updateEditorContent() = {
//      aceEditor match {
//        case Some(editor) => fetchAndUseSourceCode(s => editor.setValue(s))
//        case None => println("Strange, someone asked to update the aceEditor while there is none")
//      }
//    }

    def getEditorValue = {
      aceEditor match {
        case Some(e) => e.getValue()
        case None => "[ERROR] fun getEditorValue was called while there was no aceEditor"
      }
    }

    def setEditorValue(value: String) = {
      aceEditor match {
        case Some(e) =>
//          println("Setting Ace Editor value to: " + value)
          val line = e.session.getScrollTop()
          val col = e.session.getScrollLeft()
          e.setValue(value)
          e.selection.clearSelection()
          e.session.setScrollTop(line)
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
          aceEditor.foreach(_.getSession().setScrollTop(p.lineFrom - 1))
        case None =>
      }
    }
    
    /** Returns the ID of the added marker */
    def addMarking(lineFrom: Int, colFrom: Int, lineTo: Int, colTo: Int)(timeoutms: Int): Int = {
      aceEditor match {
        case Some(editor) =>
          println(s"Adding merking at $lineFrom, $colFrom, $lineTo, $colTo of class tmp-highlight")
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
//      println("masterCallback: currentOnChangeCallback=" +currentOnChangeCallback)
//      if(currentOnChangeCallback == Standard_OnChangeCallback) {
//        aceEdOnChangeCallback_standard("useless")
//      }
//      else{
//        aceEdOnChangeCallback_doNothing("useless")
//      }
    }

    private case object Standard_OnChangeCallback extends OnChangeCallback {override val onChangeCallback: js.Function1[scala.scalajs.js.Any, Unit] = aceEdOnChangeCallback_standard _}
    private def aceEdOnChangeCallback_standard(uselessThingJustThereForTypingWithJavaScriptFunctions: scala.scalajs.js.Any) : Unit= {
//      println("AceEditor onChange callback: standard")
      SourceCodeSubmitButton.turnBackgroundRed()
//      dom.document.getElementById("sourceCodeSubmitButton").setAttribute("style", "background-color:red")
//      AjaxClient[Api].setSourceCode(getEditorValue).call().onComplete{
//        case Failure(exception) => {println("setSourceCode request failed: " + exception.getMessage)}
//        case Success(unit) => {println("setSourceCode request successful")}
//      }
//      setSourceCodeRequestsAbsorber.newRequest()
    }

    private case object DoNothing_OnChangeCallback extends OnChangeCallback {override val onChangeCallback: js.Function1[scala.scalajs.js.Any, Unit] = aceEdOnChangeCallback_doNothing _}
    private def aceEdOnChangeCallback_doNothing(uselessThingJustThereForTypingWithJavaScriptFunctions: scala.scalajs.js.Any) : Unit = {
//      Do Nothing
//      println("AceEditor onChange callback: do nothing")
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
    private def buildStringModification(webElementID: Int): StringModification = {
//      println("innerText of webElem with ID 7: "+getElementByImplicitWebProgrammingID("7")(0).innerText)
      val newValue = getElementByImplicitWebProgrammingID(webElementID.toString)(0).innerText.getOrElse("getOrElseFailed in StringModificationSubmitter")
      StringModification(webElementID, None, newValue)
    }
    def newStringModificationOfTheTextWebAttr(webElementID: Int) = {
      if (lastModification != null) {
        if (lastModification.webElementID == webElementID) {
//          This new modification is on the same webElement and on the same WebAttribute than the current one.
          lastModification = buildStringModification(webElementID)
          stopTimeout()
          launchTimeout(lastModification)
        }
      }
      else {
        lastModification = buildStringModification(webElementID)
        launchTimeout(lastModification)
      }
    }
  }

//  def includeScriptInMainTemplate(scriptTagToInclude: scalatags.JsDom.TypedTag[org.scalajs.dom.html.Script]) = {
//    dom.document.getElementById("scalajsScriptInclusionPoint").appendChild(scriptTagToInclude.render)
//  }
  def renderWebPage(webPageWithIDedWebElements: WebPageWithIDedWebElements, destinationDivID: String) = {
    println("Rendering " + webPageWithIDedWebElements)
    val webPageDiv = <.div(
      ^.id := "webPage",
      convertWebElementWithIDToReactElement(webPageWithIDedWebElements.main)
    )
    ReactDOM.render(webPageDiv, document.getElementById(destinationDivID))
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
    def generateTextChangeCallback(webElID: Int) = {
      Callback{
        StringModificationSubmitter.newStringModificationOfTheTextWebAttr(webElID)
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
//    val textChangeCallBack = Callback{
//      StringModificationSubmitter.newStringModificationOfTheTextWebAttr(webElID)
//    }
    webElWithID match {
      case WebElementWithID(webElem, webElID) =>
        webElem match {
          case TextElement(text) =>
            val textChangeCallback = generateTextChangeCallback(webElID)
            <.span(splitTextIntoReactNodeSeq(text),
              reservedAttributeForImplicitWebProgrammingID := webElID,
              ^.contentEditable := "true",
              ^.onChange --> textChangeCallback,
              ^.onInput --> textChangeCallback,
              ^.title := "webElID= "+webElID
            )
          case Element(tag, sons, attributes, styles) =>
            tag.reactTag(
              reservedAttributeForImplicitWebProgrammingID := webElID,
              leonListToList(sons).map(convertWebElementWithIDToReactElement),
              ^.title := "webElID= "+webElID,
              leonListToList(attributes).map{ x => x.attributeName.reactAttr := x.attributeValue },
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