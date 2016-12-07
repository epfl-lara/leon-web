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
import shared.{Module => ModuleDescription, Main => _, _}
import shared.RepositoryState
import shared.equal.EqSyntax
import client.react.{App => ReactApp}
import client.react.Actions
import client.utils.BufferedWebSocket
import shared.messages.{Event => _, _}
import scala.scalajs.js.typedarray._
import java.nio.ByteBuffer
import org.scalajs.dom.raw.FileReader
import scalacss.ScalaCssReact._
import scalacss.mutable.StyleSheetRegistry
import scalacss.Defaults._
import js.JSConverters._

@JSExport
object MainDelayed extends js.JSApp {
  @JSExport
  def setTimeout(i: Int) = {
    Main.Server ! VerificationTimeout(i)
  }

  @JSExport
  def main(): Unit = {
    $(document).ready(Main.main _)
    
    val registry = new StyleSheetRegistry
    registry.register(GlobalStyles)
    registry.addToDocumentOnRegistration()
  }

  val editor = ace.edit("codebox");
  ace.require("ace/token_tooltip");
  editor.setTheme("ace/theme/chrome");
  editor.getSession().setMode("ace/mode/scala")
  editor.getSession().setUseWrapMode(true)
  editor.setShowPrintMargin(false);
  editor.setAutoScrollEditorIntoView();
  editor.setHighlightActiveLine(false);
  editor.getSession().setTabSize(2)
  $("#codebox").show()
}

trait LeonAPI {
  def setEditorCode(code: String, resetEditor: Boolean = true, force: Boolean = false): Unit
  def setRepositoryState(state: Option[RepositoryState]): Unit
  def modifyRepositoryState(f: RepositoryState => RepositoryState): Unit
  def getRepositoryState: Option[RepositoryState]
  def setTreatAsProject(treatAsProject: Boolean): Unit
  def sendMessage(m: MessageToServer): Unit
  def sendBuffered(m: MessageToServer): Unit
  /** The handler returns true if it could successfully handle the message. */
  def registerMessageHandler(m: MessageFromServer => Boolean): Unit
}

@JSExport("Main")
object Main extends LeonWeb with LeonAPI  {
  val m = Misc
  
  def main(): Unit = {
    js.timers.setInterval(2000) { checkDisconnectStatus() };

    connectWS()

    val reactApp = new ReactApp(this)
    reactApp.init()

    js.timers.setTimeout(4000) {
      if (!connected) {
        $("#disconnectError").hide();
        $("#connectError").show().alert();
      }
    }

  }

}

trait LeonWeb extends EqSyntax with ExplorationFactHandler with UndoRedoHandler with DemoHandler { self: Main.type =>
  import boopickle.Default._
  import shared.messages.MessageToServer._
  
  def window = g
  val editor = MainDelayed.editor
  val aceRange = ace.require("ace/range").Range;

  val hash = window.location.hash.asInstanceOf[js.UndefOr[String]]

  @JSExport val WS = !js.isUndefined(g.MozWebSocket) ? g.MozWebSocket | g.WebSocket

  @JSExport("leonSocket") var leonSocket: WebSocket = null
  
  private def _send(msg: ByteBuffer): Unit = leonSocket.send(new TypedArrayBufferOps(msg).arrayBuffer())

  def sendMessage(msg: MessageToServer): Unit = _send(Pickle.intoBytes(msg))
  def sendBuffered(msg: MessageToServer): Unit = new BufferedWebSocket(leonSocket).sendBuffered(msg)
    
  object Server {
    /** Synonym of Server ! message*/
    def send(msg: MessageToServer): Unit = Main.sendMessage(msg)

    /**Sends a message to the server */
    def !(msg: MessageToServer): Unit = Main.sendMessage(msg)

    /** Buffer the message in case the network is down */
    def sendBuffered(msg: MessageToServer): Unit = Main.sendBuffered(msg)

    /** Buffer the message in case the network is down */
    def !!(msg: MessageToServer): Unit = Main.sendBuffered(msg)

    /** Sends a message after registering a callback to handle the response. */
    /*def ![T <: MessageFromServer](msg: MessageToServerExpecting[T], callback: PartialFunction[T, Unit]): Unit = {
      Handlers.callbacks += new PartialFunction[MessageFromServer, Unit] {
        def isDefinedAt(e: MessageFromServer) = callback != null && e.isInstanceOf[T]
        def apply(e: MessageFromServer): Unit = callback(e.asInstanceOf[T])
      }
      Main.sendMessage(msg)
    }*/
  }
  
  /** Permalinks */
  $("#button-permalink").click(((self: Element, event: JQueryEventObject) => {
    if (!$(self).hasClass("disabled")) {
      Server ! StorePermaLink(editor.getValue())
      event.preventDefault()
  }}): js.ThisFunction);
  $("#button-permalink-close").click((event: JQueryEventObject) => {
    $("#permalink-value").hide()
  })
  Handlers += { case GotPermalink(link) =>
    $("#permalink-value input").value(window._leon_url + "#link/" + link)
    $("#permalink-value").show()
  }
  
  def registerMessageHandler(handler: MessageFromServer => Boolean): Unit = {
    Handlers += {
      case m: MessageFromServer if handler(m) =>
    }
  }

  val headerHeight = $("#title").height() + 20

  
  editor.getSession().on("changeScrollTop", (_: js.Any) => {
    hideHighlight();
  });

  def rangeScore(start: Position, end: Position): Double = {
    if (start.row == end.row) {
      (end.row - start.row) * 80 + end.column - start.column;
    } else {
      (end.row - start.row) * 80 + end.column - start.column;
    }
  }
  
  // Each feature is stored in the Feature dictionary
  object Feature {
    def apply(active: Boolean, displayName: String, module: Option[shared.Module], name: String): Feature = {
      val res = new Feature(active, displayName, module.map(m => Left(m)).getOrElse(Right(name)))
      module.foreach{m => FeaturesMappings.stringToModule += res.name -> m; FeaturesMappings.moduleToFeature += m -> res }
      FeaturesMappings.stringToFeature += res.name -> res
      res
    }
    def apply(active: Boolean, displayName: String, module: shared.Module): Feature = apply(active, displayName, Some(module), "")
    def apply(active: Boolean, displayName: String, name: String): Feature = apply(active, displayName, None, name)
  }
  
  object Features {
    import shared._
    def toJsObject = js.Dictionary[Feature](stringToFeature.toSeq : _*)
    def stringToModule = FeaturesMappings.stringToModule
    def moduleToFeature = FeaturesMappings.moduleToFeature
    def stringToFeature = FeaturesMappings.stringToFeature
    
    val verification = Feature(active= true, displayName= "Verification", module= Verification)
    val synthesis    = Feature(active= true, displayName= "Synthesis", module= Synthesis)
    val disambiguation = Feature(active= true, displayName="Synthesis clarification<i class=\"fa fa-lightbulb-o\" title=\"Beta version\"></i>", module= Disambiguation)
    val termination  = Feature(active= true, displayName= "Termination <i class=\"fa fa-lightbulb-o\" title=\"Beta version\"></i>", module= Termination)
    val presentation = Feature(active= false, displayName= "Presentation Mode", name= "presentation")
    val execution    = Feature(active= true, displayName= "Execution", module= Execution)
    val repair       = Feature(active= true, displayName= "Repair <i class=\"fa fa-lightbulb-o\" title=\"Beta version\"></i>", module= Repair)
    val invariant    = Feature(active= true, displayName="Resource Bounds<i class=\"fa fa-lightbulb-o\" title=\"Beta version\"></i>", module= Invariant)
    val webbuilding  = Feature(active= false, displayName="Web building<i class=\"fa fa-lightbulb-o\" title=\"Beta version\"></i>", module= WebsiteBuilder)
  }

  def togglePanel(button: String, show: Boolean): Unit = {
    val panel = "#" + $(button).attr("ref").getOrElse("")

    if (!show) {
      $(panel).hide()
      $(button).addClass("off")
    } else {
      $(panel).show()
      $(button).removeClass("off")
    }
  }

  var leonPanels =
    fromStorage[js.Dictionary[Boolean]]("leonPanels")
      .getOrElse(js.Dictionary.empty[Boolean])

  leonPanels foreach { case (button, visible) =>
    togglePanel(button, visible)
  }

  $(".menu-button").click(((self: Element, event: JQueryEventObject) => {
    val button  = "#" + $(self).attr("id").getOrElse("")
    val panel   = "#" + $(self).attr("ref").getOrElse("")
    val visible = $(panel).is(":visible")

    togglePanel(button, !visible)

    leonPanels += (button -> !visible)
    LocalStorage.update("leonPanels", JSON.stringify(leonPanels))

  }): js.ThisFunction);
  
  def toggleWeb(activate: Boolean) = {
    if(activate) {
      WebMode.activate()
    } else {
      WebMode.deactivate()
    }
  }
  
  $("#button-web").click(((self: Element, event: JQueryEventObject) => {
    toggleWeb(Features.webbuilding.toggle())
  }): js.ThisFunction)
  
  // Out of date:
  $("#button-save").click((event: JQueryEventObject) => {
    recompile()
    event.preventDefault()
  });

  def hasLocalStorage(): Boolean = {
    try {
      !js.isUndefined(window.localStorage) && window.localStorage =!= null;
    } catch {
      case e: Exception =>
        false
    }
  }

  //def handlers = Handlers.asInstanceOf[js.Dictionary[Any]]

  var compilationStatus = 0
  val searchFinished = false
  var context = "unknown";

  val maxHistory = 20;

  def fromStorage[A](key: String): Option[A] =
    LocalStorage(key)
      .flatMap(x => x.asInstanceOf[js.UndefOr[String]].toOption)
      .map(x => 
        try {
          JSON.parse(x).asInstanceOf[A]
        } catch {
          case e: Throwable => // May be a string.
            x.asInstanceOf[A]
        }
      )

  case class Persistent[T](name: String, defaultValue: T) {
    def get() = fromStorage[T](name)
    def apply() = get().getOrElse(defaultValue)
    def :=(v: T): Unit = LocalStorage.update(name, JSON.stringify(v.asInstanceOf[js.Any]))
  }
  implicit def convertPersistentToValue[T](p: Persistent[T]): T = p()
  
  def storeCurrent(code: String): Unit = {
    forwardChanges = new js.Array[String]()
    if (backwardChanges.length >= 1) {
      if (code =!= backwardChanges(backwardChanges.length - 1)) {
        backwardChanges.push(code)
      }
    } else {
      backwardChanges.push(code)
    }
    updateUndoRedo()
  }

  /** Compilation
    */

  def updateCompilationProgress(percents: Int): Unit = {
    $("#overview .progress-bar").css("width", percents + "%");
  }
  Handlers += { case data: HCompilationProgress => updateCompilationProgress(Math.round((data.current * 100) / data.total)) }


  def compilation(data: HCompilation) = {
    if (data.status == "success") {
      updateCompilationStatus("success")
    } else {
      updateCompilationStatus("failure")
    }
  }
  Handlers += { case data: HCompilation => compilation(data) }
  
  def updateCompilationStatus(status: String): Unit = {
    val e = $("#overview .compilation-status")
    val codebox = $("div#codebox")
    val boxes = $(".results_table")

    e.attr("class", "compilation-status");
    $(".results_table > .overlay").remove();

    if (status == "success") {
      compilationStatus = 1

      e.addClass("success")
      e.html("""Compiled <i class="fa fa-check" title="Compilation succeeded"></i>""")

      codebox.removeClass("compilation-error")
    } else if (status == "failure") {
      compilationStatus = -1

      e.addClass("failure")
      e.html("""Compilation Failed <i class="fa fa-warning" title="Compilation failed"></i>""")

      codebox.addClass("compilation-error")

      boxes.append("""<div class="overlay" />""")
      
      customGutterDecorations.clear()
      updateCustomGutterDecorations()
      
    } else if (status == "disconnected") {
      compilationStatus = 0

      e.addClass("failure")
      e.html("""Disconnected <i class="fa fa-unlink"></i>""")

      boxes.append("""<div class="overlay" />""")

    } else if (status == "unknown") {
      compilationStatus = 0

      e.html("""Compiling <i class="fa fa-refresh fa-spin" title="Compiling..."></i>""")
    } else {
      g.alert("Unknown status: " + status)
    }

    if (status == "unknown") {
      updateCompilationProgress(0);
    } else {
      updateCompilationProgress(100);
    }

    clearExplorationFacts();
    drawSynthesisOverview()
  }
  
  /** Invariants */
  
  def updateInvariantProgress(percents: Int): Unit = {
    $("#invariant .progress-bar").css("width", percents + "%");
  }

  val localFeatures = fromStorage[js.Dictionary[Feature]]("leonFeatures")

  localFeatures foreach { locFeatures =>
    for ((f, locFeature) <- locFeatures) {
      Features.stringToFeature.get(f) match {
        case Some(feature) =>
          feature.active = locFeature.active
        case None =>
      }
    }
  }
  toggleWeb(Features.webbuilding.active)
  
  val leonCommandLineFlags = Persistent("leonCommandLineFlags", "")
  if(leonCommandLineFlags() != "") {
    sendBuffered(SetCommandFlags(leonCommandLineFlags()))
  }
   
  def regenerateFeaturesPanel() = {
    val fts = $("#params-panel ul").empty()
    for ((f, feature) <- Features.stringToFeature) {
      fts.append("""<li><label class="checkbox"><input id="feature-"""" + f + " class=\"feature\" ref=\"" + f + "\" type=\"checkbox\"" + (feature.active ? """ checked="checked"""" | "") + ">" + feature.name + "</label></li>")
    }
    $("#additionalclflags")
      .value(leonCommandLineFlags())
      .off("keyup.updateServer paste.updateServer")
      .on("keyup.updateServer paste.updateServer", () => {
        val cl = $("#additionalclflags").value().asInstanceOf[String]
        leonCommandLineFlags := cl
        Server ! SetCommandFlags(cl)
      })
    
    $(".feature").click(((self: Element) => {
      val f = $(self).attr("ref").getOrElse("")
      val feature = Features.stringToFeature(f)
      val active = feature.toggle()
      if(feature.name == Features.webbuilding.name) {
        toggleWeb(active)
      }
  
      recompile(true)
  
      drawOverView()
      drawSynthesisOverview()
      setPresentationMode()
    }): js.ThisFunction)
  }
  regenerateFeaturesPanel()

  setPresentationMode()

  type ModulesMap = scala.collection.mutable.Map[String, Module]

  abstract class Module(val module: ModuleDescription, list: ModulesMap) { self =>
    val column: String
    def html(name: String, d: Status): HandlerMessages.Html
    def missing(name: String): HandlerMessages.Html
    def handlers(): Unit
    list += module.name -> self
  }

  object overview {

    object modules {
      val list = scala.collection.mutable.Map[String, Module]() // Defined before all modules.

      val verification = new Module(shared.Verification, list) {
        val column = "Verif."
        def html(name: String, d: Status): HandlerMessages.Html = {
          val vstatus = d.status match {
            case VerifStatus.crashed =>
              """<i class="fa fa-bolt text-danger" title="Unnexpected error during verification"></i>"""
            case VerifStatus.undefined =>
              """<i class="fa fa-refresh fa-spin" title="Verifying..."></i>"""
            case VerifStatus.cond_valid =>
              """<span class="text-success" title="Conditionally valid">(<i class="fa fa-check"></i>)</span>"""
            case VerifStatus.valid =>
              """<i class="fa fa-check text-success" title="Valid"></i>"""
            case VerifStatus.invalid =>
              """<i class="fa fa-exclamation-circle text-danger" title="Invalid"></i>""";
            case VerifStatus.timeout =>
              """<i class="fa fa-clock-o text-warning" title="Timeout"></i>"""
            case _ =>
              """<i class="fa fa-refresh fa-spin" title="Verifying..."></i>"""
          }

          "<td class=\"status verif\" fname=\"" + name + "\">" + vstatus + "</td>"
        }
        def missing(name: String): HandlerMessages.Html = {
          "<td class=\"status verif\" fname=\"" + name + "\"><i class=\"fa fa-question\" title=\"unknown\"></i></td>"
        }
        def handlers(): Unit = {
          $("td.verif").click(((self: Element) => {
            val fname = $(self).attr("fname").getOrElse("")
            overview.Data.verification.get(fname) match {
              case Some(d) =>
                openVerifyDialog()
                displayVerificationDetails(d.fname, d.status, d.vcs, d.crashingInputs)
              case None =>
                openVerifyDialog()
                displayVerificationDetails(fname, "unknown", Array[VC](), None)
            }
          }): js.ThisFunction)
        }
      }
      val termination = new Module(Termination, list) {
        val column = "Term."
        def html(name: String, d: Status): HandlerMessages.Html = {
          val tstatus = d.status match {
            case TerminationStatus.wip =>
              """<i class="fa fa-refresh fa-spin" title="Checking termination..."></i>""";
            case TerminationStatus.terminates =>
              """<i class="fa fa-check text-success" title="Termination guaranteed"></i>""";
            case TerminationStatus.loopsfor =>
              """<i class="fa fa-exclamation-circle text-danger" title="Non-terminating"></i>""";
            case TerminationStatus.callsnonterminating =>
              """<span class="text-success" title="Calls non-terminating functions">(<i class="fa fa-check text-success"></i>)</span>""";
            case TerminationStatus.noguarantee =>
              """<i class="fa fa-question text-danger" title="No termination guarantee"></i>""";
            case _ =>
              """<i class="fa fa-question" title="Unknown" />""";
          }

          "<td class=\"status termin\" fname=\"" + name + "\">" + tstatus + "</td>"
        }
        def missing(name: String): HandlerMessages.Html = {
          "<td class=\"status termin\" fname=\"" + name + "\"><i class=\"fa fa-question\" title=\"unknown\"></i></td>"
        }
        def handlers(): Unit = {
          $("td.termin").click(((self: Element) => {
            val fname = $(self).attr("fname").getOrElse("")
            openTerminationDialog()
            overview.Data.termination.get(fname) match {
              case Some(d) =>
                displayTerminationDetails(d.status, d)
              case None =>
                displayTerminationDetails("unknown", null)
            }
          }): js.ThisFunction);
        }
      }
      val invariant = new Module(Invariant, list) {
        val column = "Inv."
        def html(name: String, d: Status): HandlerMessages.Html = {
          val istatus = d.status match {
            case InvariantStatus.crashed =>
              """<i class="fa fa-bolt text-danger" title="Unnexpected error during verification"></i>"""
            case InvariantStatus.found =>
              """<i class="fa fa-check text-success" title="Found invariant"></i>"""
            case InvariantStatus.timeout =>
              """<i class="fa fa-clock-o text-warning" title="Timeout"></i>"""
            case InvariantStatus.undefined | _ =>
              """<i class="fa fa-refresh fa-spin" title="Finding invariant..."></i>"""
          }
          "<td class=\"status invart\" fname=\"" + name + "\">" + istatus + "</td>"
        }
        def missing(name: String): HandlerMessages.Html = {
          "<td class=\"status invart\" fname=\"" + name + "\"><i class=\"fa fa-question\" title=\"unknown\"></i></td>"
        }
        def handlers(): Unit = {
          $("td.invart").click(((self: Element) => {
            val fname = $(self).attr("fname").getOrElse("")
            overview.Data.invariant.get(fname) match {
              case Some(d) =>
                openInvariantDialog()
                displayInvariantDetails(d.status, d, overview.Data.invariant)
              case None =>
                openInvariantDialog()
                displayInvariantDetails("unknown", l().asInstanceOf[InvariantDetails], overview.Data.invariant)
            }
          }): js.ThisFunction)
        }
      }
    }

    var functions = js.Dictionary.empty[OverviewFunction]
    @ScalaJSDefined
    object Data extends js.Object {
      var verification = Map[String, VerificationDetails]()
      var termination = Map[String, TerminationDetails]()
      var invariant = Map[String, InvariantDetails]()
    }
  }
  Handlers += { case data: HUpdateOverview =>
    overview.functions = js.Dictionary.empty[OverviewFunction];

    for ((i, fdata) <- data.overview) {
      val fdata = data.overview(i)
      val fname = fdata.name
      overview.functions(fname) = fdata
    }
    repair_started = false
  }
  
  var repair_started: Boolean = false
  var repair_result_fname: js.UndefOr[String] = js.undefined
  def maybeUpdateRepairDialog() = {
    val c = overview.Data.verification
    c.get(repair_result_fname.getOrElse("")) match {
      case Some(vc) =>
        if($("#repairDialog").is(":visible") && !repair_started) {
          if(vc.status == VerifStatus.invalid) {
            repair_started = true
            Backend.repair.doRepair(repair_result_fname.getOrElse(""))
          }
        }
      case None =>
    }
  }

  Handlers += { case data: HUpdateVerificationOverview =>
    overview.Data.verification = data.overview

    drawOverView()
    drawVerificationOverviewInGutter()
    maybeUpdateRepairDialog()
  }
  Handlers += { case data: HUpdateTerminationOverview =>
    overview.Data.termination = data.overview
    drawOverView()
  }
  Handlers += { case data: HUpdateInvariantsOverview =>
    overview.Data.invariant = data.overview
    drawOverView()
  }
  
  var synthesisOverview: SynthesisOverview = SynthesisOverview(None)
  var synthesis_chosen_rule: Option[String] = None
  var synthesis_result_fname: js.UndefOr[String] = js.undefined
  var synthesisOverview_rulesToApply: Map[(String, Int),  HSynthesisRulesToApply] = Map()
    
  lazy val mountedSynthesisTable = {
    val panelSynthesisTable = $("#synthesis .results_table")(0).asInstanceOf[dom.Node]
    if (panelSynthesisTable =!= null) {
      Some(ReactDOM.render(SynthesisOverviewTable(() => Main.overview.functions, synthesis_chosen_rule = _), panelSynthesisTable))
    } else None
  }
  
  Handlers += { case data: SynthesisOverview =>
    if (synthesisOverview.toString != data.toString) {
      synthesisOverview = data;
      synthesisOverview_rulesToApply = Map();
      drawSynthesisOverview();
      
      Main.onSynthesisTabDisplay match {
        case Some(handler) => handler()
          Main.onSynthesisTabDisplay = None
        case None =>
      }
      
      val hasFunctions = data.functions.isDefined && data.functions.get.keys.nonEmpty
      if (hasFunctions && Features.synthesis.active) {
        if($("#synthesisDialog").is(":visible") && compilationStatus == 1) { // Automatic retrieval of rules if the synthesis dialog is visible.
          val fname = (synthesis_result_fname.getOrElse(""): String)
          val cid =  $("#synthesis_table td.fname[fname="+fname+"]").attr("cid").getOrElse("0").toInt
          console.log("Finding rules to apply 2 " + new js.Date())
          Backend.synthesis.getRulesToApply(fname, cid)
        }
      }
    }  
  }
  def synthesis_rulesToApply(data: HSynthesisRulesToApply) = {
    synthesisOverview_rulesToApply += (data.fname, data.cid) -> data
    drawSynthesisOverview()
    
    // Automatic re-search or re-application of rule.
    if($("#synthesisDialog").is(":visible") && (Main.synthesis_result_fname.getOrElse("") == data.fname)) { // Was not closed maybe because of disambiguation. Relaunch synthesis for the same method.
      val cid =  $("#synthesis_table td.fname[fname="+data.fname+"]").attr("cid").getOrElse("0").toInt
      synthesis_chosen_rule match {
        case None => // Explore
        case Some("search") => // Search
          Backend.synthesis.search(synthesis_result_fname.getOrElse(""), cid)
        case Some(rid) => // Rule chosen
          Backend.synthesis.doApplyRule(data.fname, cid, rid.toInt)
      }
    }
  }
  Handlers += { case data: HSynthesisRulesToApply => synthesis_rulesToApply(data) }

  def drawSynthesisOverview(): Unit = {
    val data = synthesisOverview
    mountedSynthesisTable.foreach(
      _.backend.changeState(compilationStatus == 1, data, synthesisOverview_rulesToApply)
    )

    val hasFunctions = data.functions.isDefined && data.functions.get.keys.nonEmpty
    
    if (hasFunctions && Features.synthesis.active) {
      $("#synthesis").show()
    } else {
      $("#synthesis").hide()
    }
    
    $("#invariant").hide()
  }

  def setPresentationMode(): Unit = {
    if (Features.presentation.active) {
      $("body").addClass("presentation")
    } else {
      $("body").removeClass("presentation")
    }
    resizeEditor()
  }
  val possibleGutterMarkers = Set(
    "fa", "fa-bolt", "text-danger", "fa-check", "text-success", "fa-exclamation-circle", "text-danger", "fa-clock-o", "text-warning", "fa-refresh"
  )
  
  val priorities = Map(
      "fa-exclamation-circle" -> 5,
      "fa-bolt" -> 4,
      "fa-clock-o" -> 3,
      "text-success" -> 2,
      "fa-refresh" -> 1).withDefault { x => 0 }
  
  case class CustomGutterDecoration(row: Int, classes: Set[String]=Set(), html: String, callback: () => Unit)
  
  var customGutterDecorations = collection.mutable.Map[Int, CustomGutterDecoration]()
  
  def updateCustomGutterDecorations() = {
    //println("Updating custom gutter decorations")
    $("#codebox div.ace_gutter-cell .custommarker").remove()
    $("#codebox div.ace_gutter-cell").each((i: Int, elem: dom.Element) => {
      val row = elem.textContent.toInt
      customGutterDecorations.get(row) match {
        case None =>
          $(elem)
        case Some(CustomGutterDecoration(_, classes, html, callback)) =>
          $(elem).prepend($(html).addClass("custommarker").click(() => callback()))
      }
      ().asInstanceOf[js.Any]
    })
  }
  
  def drawVerificationOverviewInGutter(): Unit = {
    val verification = overview.modules.verification
    val details = overview.Data.verification
    val session = Main.editor.getSession()
    val decorations = collection.mutable.Map[Int, CustomGutterDecoration]()
    for((k, v) <- details) {
      val vstatus = v.status
      for(vc <- v.vcs) {
        val status = if(vstatus == VerifStatus.crashed) vstatus else vc.status
        val classStatus: Set[String] = status match {
          case VerifStatus.crashed =>
            Set("fa", "fa-bolt", "text-danger")
          case VerifStatus.valid =>
            Set("fa", "fa-check", "text-success")
          case VerifStatus.invalid =>
            Set("fa", "fa-exclamation-circle", "text-danger")
          case VerifStatus.timeout =>
            Set("fa", "fa-clock-o", "text-warning")
          case VerifStatus.undefined | VerifStatus.unknown =>
            Set("fa", "fa-refresh"/*, "fa-spin"*/)
          //case  | V =>
          case _ => Set.empty/*
          case VerifStatus.cond_valid =>
            """fa fa-check"""
          case VerifStatus.undefined =>
            """fa fa-refresh fa-spin"""

          case VerifStatus.unknown =>
            ""*/
        }
        val html = status match {
          case VerifStatus.crashed =>
            """<i class="fa fa-bolt text-danger" title="Unnexpected error during verification"></i>"""
          case VerifStatus.undefined =>
            """<i class="fa fa-refresh fa-spin" title="Verifying..."></i>"""
          case VerifStatus.cond_valid =>
            """<span class="text-success" title="Conditionally valid">(<i class="fa fa-check"></i>)</span>"""
          case VerifStatus.valid =>
            """<i class="fa fa-check text-success" title="Valid"></i>"""
          case VerifStatus.invalid =>
            """<i class="fa fa-exclamation-circle text-danger" title="Invalid"></i>""";
          case VerifStatus.timeout =>
            """<i class="fa fa-clock-o text-warning" title="Timeout"></i>"""
          case _ =>
            """<i class="fa fa-refresh fa-spin" title="Verifying..."></i>"""
        }
        val priority = (0 :: (classStatus collect priorities).toList).max
        //println(s"Adding $classStatus (status = ${vc.status}) between ${vc.lineFrom} and ${vc.lineTo} for ${vc.fun} [kind = ${vc.kind}]")
        for{row <- vc.lineFrom to vc.lineTo} {
          val existing = decorations.get(row).map(_.classes).getOrElse(Set.empty)
          val existing_priority = (0 :: (existing collect priorities).toList).max
          if(existing_priority < priority) {
            decorations(row) = CustomGutterDecoration(row, classStatus, html, () => {
              $(s"td.verif[fname=${vc.fun}]").click()
            })
          } else {
            //println(s"Discarded adding line $row because lower priority than " + existing)
          }
        }
      }
    }
    /*for(row <- 0 until session.getLength().toInt) {
      for(p <- possibleGutterMarkers) {
        session.removeGutterDecoration(row, p)
      }
    }*/
    //println("Finished. Adding " + decorations.size + " decorations")
    customGutterDecorations = decorations/*js.Array[CustomGutterDecoration]()
    for{(row, decoration) <- decorations.toSeq.sortBy(_._1)} {
      customGutterDecorations.push(decoration)
      //session.addGutterDecoration(row, cl)
    }*/
    updateCustomGutterDecorations()
  }
 
  Handlers += { case data: HEditor =>
    if (data.annotations.isDefined) {
      val annotations = data.annotations.get
      val session = Main.editor.getSession();

      context = "unknown";

      $("#annotations").html("");

      for (a <- annotations) {
        if (a.tpe == "verification") {
          context = "verification";
        } else if (a.tpe == "synthesis") {
          context = "synthesis";
        }
        var tpe = a.tpe.kind
        
        if (tpe != "info" && tpe != "error" && tpe != "warning") {
          session.addGutterDecoration(a.line, "leon_gutter_" + tpe)
          tpe = "info";
        }

        if (tpe == "error") {
          val line = a.line + 1
          $("#annotations").append("<li class=\"clicktoline error\" line=\"" + line + "\"><code><i class=\"fa fa-warning\"></i> " + line + ":" + a.message + "</code></li>")
        } else if (tpe == "warning") {
          val line = a.line + 1
          $("#annotations").append("<li class=\"clicktoline warning\" line=\"" + line + "\"><code><i class=\"fa fa-warning\"></i> " + line + ":" + a.message + "</code></li>")
        }
      }

      addClickToLine("#annotations");
      session.setAnnotations(
          annotations.map(a =>
            l(row = a.line-1, column = a.col-1, text = a.message, `type` = a.tpe.kind).asInstanceOf[com.scalawarrior.scalajs.ace.Annotation]).toJSArray);
      resizeEditor();
    }
  }

  def drawOverView(): Unit = {
    val overview_table = $("#overview_table")
    var html: HandlerMessages.Html = "";

    html += "<tr>"
    html += "<th>Function</th>"
    for ((name, module) <- overview.modules.list) {
      if (Features.stringToFeature(name).active) {
        html += "<th>" + module.column + "</th>"
      }
    }
    html += "</tr>"

    for ((fname, fdata) <- overview.functions.toSeq.sortBy(_._1)) {
      val fdata = overview.functions(fname)

      html += "<tr>"
      html += "  <td class=\"fname clicktoline\" line=\"" + fdata.line + "\">" + fdata.displayName + "</td>"
      for ((m, mod) <- overview.modules.list) {
        if (Features.moduleToFeature(mod.module).active) {
          val data = overview.Data.asInstanceOf[js.Dictionary[Map[String, Status]]](m)
          data.get(fname) match {
            case Some(status) =>
              html += mod.html(fname, status)
            case _ =>
              html += mod.missing(fname)
          }
        }
      }
      html += "</tr>"
    }

    overview_table.html(html);

    for ((name, m) <- overview.modules.list) {
      m.handlers()
    }

    addClickToLine("#overview_table");
    addHoverToLine("#overview_table");

    if (js.Object.keys(overview.functions.asInstanceOf[js.Object]).length == 0) {
      overview_table.hide()
    } else {
      overview_table.show()
    }
  }

  def addClickToLine(within: String): Unit = {
    $(within + " .clicktoline[line]").click(((_this: Element) => {
      val line = $(_this).attr("line").getOrElse("0").toDouble
      editor.gotoLine(line);
    }): js.ThisFunction)
  }

  def addHoverToLine(within: String): Unit = {
    $("").click(((_this: Element, event: JQueryEventObject) => {
    }): js.ThisFunction)

    $(within + " .hovertoline[line]").hover((((_this: Element, event: JQueryEventObject) => {
      val line = $(_this).attr("line").getOrElse("0").toDouble
      editor.gotoLine(line).asInstanceOf[js.Any]
    }): js.ThisFunction).asInstanceOf[js.Function1[org.scalajs.jquery.JQueryEventObject, scala.scalajs.js.Any]], handlerOut = (event: JQueryEventObject) => ().asInstanceOf[js.Any])
  }

  def allowPrettyPrintingSynthesis(fname: String, outputs: js.Array[DualOutput], tbl: JQuery) = {
    // Pretty-printing options.
    tbl.find("div.output")
    .each((i: Int, elem: dom.Element) => {
      //e.target.
      //TODO: Display the "validate" and "cancel" button
      //val editbox = $("""<i class="fa fa-pencil-square-o"></i>""").text("edit").hide().
      val validateBox = $("""<i class="fa fa-check save-expr-display mini-menu"></i>""").text("Confirm").attr("title", "Creates a new pretty-printer using the provided example")
      val cancelBox = $("""<i class="fa fa-times cancel-expr-display mini-menu"></i>""").text("Cancel").attr("title", "Cancel the edition of this example")
      val originalBox = $("""<i class="fa fa-eye original-expr-display mini-menu"></i>""").text("Show original").attr("title", "Show the original. Click again to return to editing")
      val menuBox = $("<div>").addClass("menu-expr-display").append(validateBox).append(cancelBox).append(originalBox).hide()
      menuBox.appendTo($(elem).parent())
      
      validateBox.on("click", () => {
        val dualOutput = outputs(i)
        console.log($(elem)(0))
        val newContent = $(elem)(0).innerText.orIfNull($(elem)(0).textContent)
        dualOutput.modifying = newContent.toOption
        console.log("Sending synthesis problem", dualOutput.toString())
        console.log("Fname = ", fname)
        onSynthesisTabDisplay = Some(() => Main.showContextDemo(Main.demoSynthesizePrettyPrinter))
        //TODO: Do something with the dual output
        Backend.verification.prettyPrintCounterExample(newContent.getOrElse(""), dualOutput.rawoutput, fname)
        menuBox.hide()
      })
      cancelBox.on("click", () => {
        val dualOutput = outputs(i)
        dualOutput.modifying = Some(dualOutput.prettyoutput)
        $(elem).text(dualOutput.prettyoutput)
        menuBox.hide()
        $(elem).blur()
      })
      // Focus/blur is not very robust. Here is what it does (if it needs to be extended later)
      // If the user focuses the div.output, the menu appears and any disappearance of the menu is cancelled immediately.
      // If the user unfocuses the div.output, the menu disappear after 10ms
      // If the user focuses the originalBox, the menu disappearance is cancelled immediately
      // If the user unfocuses the originalBox, the menu disappear after 10ms
      
      originalBox.on("click", () => {
        js.timers.clearTimeout($(elem).data("hidehandler").asInstanceOf[js.timers.SetTimeoutHandle])
        console.log("cLicked on originalBox " + i)
        val dualOutput = outputs(i)
        if(originalBox.hasClass("checked")) {
          originalBox.removeClass("checked")
          $(elem).attr("contentEditable", true)
          $(elem).text(dualOutput.modifying.getOrElse(dualOutput.prettyoutput))
        } else {
          originalBox.addClass("checked")
          $(elem).attr("contentEditable", false)
          dualOutput.modifying = $(elem)(0).innerText.orIfNull($(elem)(0).textContent).toOption
          $(elem).text(dualOutput.rawoutput)
        }
      }).on("blur", () => { // If not clicking on the div.output, hide the menu.
        console.log("Blurring originalBox...")
        $(elem).data("hidehandler", js.timers.setTimeout(100){
          $(elem).parent().find(".menu-expr-display").hide("blind")
        })
      })
    })
    .focus((e: JQueryEventObject) => {
      console.log("Focusing...")
      js.timers.clearTimeout($(e.target).data("hidehandler").asInstanceOf[js.timers.SetTimeoutHandle])
      $(e.target).parent().find(".menu-expr-display").show("blind")
    })
    .on("blur", (e: JQueryEventObject) => {
      console.log("Blurring div.output")
      $(e.target).data("hidehandler", js.timers.setTimeout(100){
        console.log("Atual blur")
        $(e.target).parent().find(".menu-expr-display").hide("blind")
      })
    })
  }

  var synthesizing = false;

  def displayVerificationDetails(fname: String, status: String, vcs: HandlerMessages.VCS, crashingInputs: Option[Map[String, DualOutput]] = None): Unit = {
    val pb = $("#verifyProgress")
    val pbb = pb.children(".progress-bar")

    pbb.width("100%")
    pb.removeClass("active")
    pb.addClass("progress-bar-striped")

    pbb.removeClass("progress-bar-warning progress-bar-success progress-bar-danger")

    var canRepair = false

    status match {
      case VerifStatus.crashed =>
        pbb.html("Crashed!")
        pbb.addClass("progress-bar-danger")

      case VerifStatus.cond_valid =>
        pbb.html("Conditionally Valid!")
        pbb.addClass("progress-bar-warning")

      case VerifStatus.valid =>
        pbb.html("Valid!")
        pbb.addClass("progress-bar-success")

      case VerifStatus.invalid =>
        pbb.html("Invalid!")
        pbb.addClass("progress-bar-danger")
        canRepair = true

      case VerifStatus.unknown =>
        pbb.html("Unknown ?!")
        pbb.addClass("progress-bar-warning")

      case VerifStatus.timeout =>
        pbb.html("Timeout!")
        pbb.addClass("progress-bar-warning")

      case _ =>
    }

    val tbl = $("#verifyResults tbody")
    tbl.html("");
    
    val outputs = js.Array[DualOutput]() // For each of these elements, a <div class="output"> must be created

    crashingInputs match {
      case Some(args) =>
        var clas = "danger"
        var html = "<tr class=\"" + clas + " counter-example\"><td colspan=\"4\">"
        html += "<div>"
        html += "  <p>The following inputs crashed the function:</p>";
        html += "  <table class=\"input\">";
        var suggestEdit = false
        for((fname, value) <- args) { 
          suggestEdit = suggestEdit || (value.prettyoutput == value.rawoutput && value.rawoutput.indexOf(",") >= 0)
          html += "<tr><td>" + fname + "</td><td>&nbsp;:=&nbsp;</td><td><div class='output' contentEditable='true' tabindex=0>" + value.prettyoutput + "</div></td></tr>";
          outputs.push(value)
        }
        html += "  </table>"
        html += "    </div>"
        html += "  </td>"
        html += "</tr>"
        tbl.append(html)
        
      case None =>
    }
    
    
    
    for (i <- 0 until vcs.length) {
      val vc = vcs(i)
      var icon = "check"
      if (vc.status == "invalid" || vc.status == "crashed") {
        icon = "warning"
      } else if (vc.status == "unknown") {
        icon = "question"
      } else if (vc.status == "timeout") {
        icon = "clock-o"
      }

      var clas = "success"
      if (vc.status == "invalid" || vc.status == "crashed") {
        clas = "danger"
      } else if (vc.status == "unknown" || vc.status == "timeout") {
        clas = "warning"
      }

      tbl.append("<tr class=\"" + clas + "\"> <td>" + vc.fun + "</td> <td>" + vc.kind + "</td> <td><i class=\"fa fa-" + icon + "\"></i> " + vc.status + "</td> <td>" + vc.time + "</td> </tr>")

      if (vc.counterExample.isDefined) {
        /*<.tr(^.classSet(clas -> true, "counter-example" -> true),
          <.td(^.colSpan := 4,
            <.div(
              <.p("The following inputs violate the VC:"),
              <.table(^.className := "input",
                for((variable_name, value) <- vc.counterExample.get) yield { 
                  <.tr(<.td(variable_name), <.td("&nbsp;:=&nbsp"), <.td(<.div(^.className := "output", ^.contentEditable := true, value)));
                }
              )
              // TODO : Continue
            )
          )
        )*/
        
        
        var html = "<tr class=\"" + clas + " counter-example\"><td colspan=\"4\">"
        html += "<div>"
        html += "  <p>The following inputs violate the VC:</p>";
        html += "  <table class=\"input\">";
        var suggestEdit = false
        for((fname, value) <- vc.counterExample.get) { 
          suggestEdit = suggestEdit || (value.prettyoutput == value.rawoutput && value.rawoutput.indexOf(",") >= 0)
          html += "<tr><td>" + fname + "</td><td>&nbsp;:=&nbsp;</td><td><div class='output' contentEditable='true' tabindex=0>" + value.prettyoutput + "</div></td></tr>";
          outputs.push(value)
        }
        html += "  </table>"

        if (vc.execution.isDefined && vc.execution.get.result == "success" && Features.execution.active) {
          html += "  <p>It produced the following output:</p>";
          html += "  <table class=\"input\">";
          html += "  "+ "<tr><td>" + "<div class='output'  contentEditable='true' tabindex=0>" + vc.execution.get.output.get.prettyoutput + "</div>" + "</td></tr>"
          html += "  </table>"
          vc.execution.get.output.foreach(o => outputs.push(o))
        }

        html += "    </div>"
        html += "  </td>"
        html += "</tr>"

        tbl.append(html)
        
        if(suggestEdit) Main.showContextDemo(Main.demoEditCounterExamples)
      }
    }
    allowPrettyPrintingSynthesis(fname, outputs, tbl)

    if (vcs.length == 0) {
      tbl.append("<tr class=\"empty\"><td colspan=\"4\"><div>No VC found</div></td></tr>")
    }

    $("div[aria-describedby='verifyDialog'] span.ui-button-text").html("Close")

    if (canRepair && Features.repair.active) {
      $(".repairButton").unbind("click").click(() => {
        Backend.repair.doRepair(fname)

        $("#verifyDialog").modal("hide")
      });
      $(".repairButton").show();
    } else {
      $(".repairButton").hide();
    }

    $("#verifyResults").show("fade");
  }
  
  def displayInvariantDetails(status: String,
      invariant: InvariantDetails,
      all_invariants: Map[String, InvariantDetails]): Unit = {
    
    val pb = $("#invariantProgress")
    val pbb = pb.children(".progress-bar")

    pbb.width("100%")
    pb.removeClass("active")
    pb.addClass("progress-bar-striped")

    pbb.removeClass("progress-bar-warning progress-bar-success progress-bar-danger")

    status match {
      case InvariantStatus.crashed =>
        pbb.html("Internal error")
        pbb.addClass("progress-bar-warning")

      case InvariantStatus.found =>
        pbb.html("Invariant found!")
        pbb.addClass("progress-bar-success")

      case InvariantStatus.invalid =>
        pbb.html("Invalid invariant template!")
        pbb.addClass("progress-bar-danger")

      case InvariantStatus.unknown =>
        pbb.html("Unknown ?!")
        pbb.addClass("progress-bar-warning")

      case InvariantStatus.timeout =>
        pbb.html("Timeout!")
        pbb.addClass("progress-bar-warning")

      case _ =>
    }
    
    val tbl = $("#invariantResults tbody")
    tbl.html("");
    
    def formatInvariant(s: String): String = {
      s.replaceAll("<=", "≤")
      .replaceAll(">=", "≥")
      .replaceAll("!=", "≠")
    }

    var targetFunction: String = null
    //for (invariant <- invariants) {
      val icon = invariant.status match {
        case InvariantStatus.crashed => "question"
        case InvariantStatus.invalid => "status"
        case InvariantStatus.timeout => "clock-o"
        case InvariantStatus.found | _ => "check"
      }

      val clas = invariant.status match {
        case InvariantStatus.invalid => "danger"
        case InvariantStatus.unknown | InvariantStatus.timeout => "warning"
        case _ => "success"
      }

      tbl.append("<tr class=\"" + clas + "\"> <td>" +
          invariant.fun + "</td> <td>" +
          formatInvariant(invariant.oldInvariant) + "</td> <td>" +
          formatInvariant(invariant.newInvariant) + "</td> <td>" +
          "<i class=\"fa fa-" + icon + "\"></i> " + status + "</td><td>" +
          invariant.time + "s </td>"+
          "</tr>")
    //}

    /*if (invariants.length == 0) {
      tbl.append("<tr class=\"empty\"><td colspan=\"4\"><div>No invariant found</div></td></tr>")
    }*/

    $("div[aria-describedby='invariantDialog'] span.ui-button-text").html("Close")

    $("#invariantResults").show("fade");
    
    
    $("#invariantDialog .importButton").show()
    $("#invariantDialog .cancelButton").show()
    
    $("#invariantDialog .importButton").unbind("click").click(() => {
      Misc.replace_code(invariant.newCode)
    })
    val code = all_invariants.get(Constants.invariantMainCode) match {
      case Some(result) =>
        $("#invariantDialog .importAllButton").unbind("click").click(() => {
          Misc.replace_code(result.newCode)
        })
        $("#invariantDialog .importAllButton").show()
      case _ =>
        $("#invariantDialog .importAllButton").hide()
    }
    
    $("#invariantDialog").modal("show")
  }

  def displayTerminationDetails(
    status: String,
    fdata: TerminationDetails): Unit = {
    val pb = $("#terminationProgress")
    val pbb = pb.children(".progress-bar")

    pbb.width("100%")
    pb.removeClass("active")
    pb.addClass("progress-bar-striped")

    pbb.removeClass("progress-bar-warning progress-bar-success progress-bar-danger")

    val tbl = $("#terminationResults table")
    tbl.html("");

    status match {
      case TerminationStatus.terminates =>
        pbb.html("Terminates!")
        pbb.addClass("progress-bar-success")
        val reason = fdata.reason.getOrElse("")
        tbl.append("""<tr class="success"> <td>This function terminates for all inputs."""+(if(reason =!= "") " (" + reason + ")" else "")+"""</td> </tr>""")

      case TerminationStatus.loopsfor =>
        pbb.html("Non-terminating!")
        pbb.addClass("progress-bar-danger")
        var html = """<tr class="danger counter-example"><td><div>"""
        html += "<p>The function does not terminate for the following call:</p>";
        html += "<table class=\"input\">";
        html += "  <tr><td>" + fdata.call + "</td></tr>";
        html += "</table>"
        html += "</div></td></tr>"
        tbl.append(html)

      case TerminationStatus.callsnonterminating =>
        pbb.html("Calls non-terminating functions!")
        pbb.addClass("progress-bar-warning")
        var html = """<tr class="warning counter-example"><td><div>"""
        html += "<p>The function calls the following non-terminating function(s):</p>";
        html += "<table class=\"input\">";
        for (call <- fdata.calls) {
          html += "<tr><td>" + call + "</td></tr>";
        }
        html += "</table>"
        html += "</div></td></tr>"
        tbl.append(html)

      case TerminationStatus.noguarantee =>
        pbb.html("No guarantee!")
        pbb.addClass("progress-bar-warning")
        tbl.append("""<tr class="warning"> <td>Leon could not determine whether or not this function terminates.</td> </tr>""")

      case _ =>
        pbb.html("Unknown!")
        pbb.addClass("progress-bar-warning")
    }

    $("div[aria-describedby='terminationDialog'] span.ui-button-text").html("Close")
    $("#terminationResults").show("fade");
  }

  def error(msg: String): Unit = {
    g.alert(msg)
  }

  def errorEvent(event: ErrorEvent): Unit = {
    console.log("ERROR")
    console.log(event)
  }

  @ScalaJSDefined trait Kind extends js.Object { val kind: String }

  def receiveEvent(event: MessageEvent): Unit = {
    import leon.web.shared.messages.MessageFromServer._
    import scala.scalajs.js.typedarray._
    val fileReader = new FileReader();
    fileReader.onload = (e: org.scalajs.dom.raw.UIEvent) => {
        val arrayBuffer = fileReader.result.asInstanceOf[ArrayBuffer];
        val data = Unpickle[MessageFromServer].fromBytes(TypedArrayBuffer.wrap(arrayBuffer))
        Handlers(data)
    }
    fileReader.readAsArrayBuffer(event.data.asInstanceOf[org.scalajs.dom.raw.Blob]);
  }

  var connected = false
  var isConnecting = false

  var lastReconnectDelay = 0;
  var reconnectIn = 0;

  def closeEvent(event: CloseEvent): Unit = {
    if (connected) {
      setDisconnected()
    }
  }

  def sendBufferedMessages(): Unit = {
    BufferedWebSocket.sendAll(leonSocket)
  }

  def openEvent(event: Event): Unit = {
    if (lastReconnectDelay =!= 0) {
      notify("And we are back online!", "success")
      updateCompilationStatus("unknown")
      oldCode = ""
    }

    setConnected()

    for ((module, feature) <- Features.moduleToFeature) {
      try {
        Backend.main.setFeatureActive(module, feature.active)
      } catch {
        case _: Exception => js.timers.setTimeout(500) {
          openEvent(event)
        }
      }
    }

    if (hash.isDefined && hash.get =!= "") {
      loadStaticLink(hash.get)
    } else {
      recompile()
    }
  }

  def loadStaticLink(hash: String): Unit = {
    if (hash.indexOf("#link/") == 0) {
      Backend.main.accessPermaLink(hash.substring("#link/".length))
      window.location.hash = ""
    }
    if (hash.indexOf("#weblink/") == 0) {
      Backend.main.accessPermaLink(hash.substring("#weblink/".length))
      window.location.hash = ""
      Features.webbuilding.activate()
      Features.termination.deactivate()
      Features.verification.deactivate()
      Features.execution.deactivate()
      WebMode.activate()
    }
    if (hash.indexOf("#demo/") == 0) {
      loadExample("demo", hash.substring("#demo/".length))
      window.location.hash = ""
    }
  }

  $(window).on("hashchange", () => {
    val hash = window.location.hash.asInstanceOf[String];
    loadStaticLink(hash);
  });

  def setDisconnected(): Unit = {
    connected = false
    isConnecting = false

    updateCompilationStatus("disconnected")
    lastReconnectDelay = 5;
    reconnectIn = lastReconnectDelay;

    checkDisconnectStatus()
  }

  def setConnected(): Unit = {
    connected = true
    isConnecting = false

    $("#connectError").hide();
    $("#disconnectError").hide();

    lastReconnectDelay = 0;
    reconnectIn = -1;

    sendBufferedMessages()
  }

  def checkDisconnectStatus(): Unit = {
    if (reconnectIn == 0) {
      reconnectIn = -1;
      $("#disconnectError #disconnectMsg").html("Attempting reconnection...");

      connectWS()

      // If still not connected after 5 seconds, consider failed
      js.timers.setTimeout(5000) {
        if (!connected) {
          isConnecting = false
          if (lastReconnectDelay == 0) {
            lastReconnectDelay = 5;
          } else {
            lastReconnectDelay *= 2;
          }

          reconnectIn = lastReconnectDelay;
        }
      }
    } else if (reconnectIn > 0) {
      $("#disconnectError #disconnectMsg").html("Retrying in " + reconnectIn + """ seconds... <button id="tryReconnect" class="btn btn-danger btn-xs">Try now</button>""");

      $("#tryReconnect").click(() => {
        reconnectIn = 0;
        checkDisconnectStatus();
      })

      $("#disconnectError").show().alert();

      reconnectIn -= 1;
    }
  }

  @JSExport
  def connectWS(): Unit = if (!isConnecting) {
    isConnecting = true

    val url = g._leon_websocket_url.asInstanceOf[String]

    leonSocket           = new WebSocket(url)
    leonSocket.onopen    = openEvent _
    leonSocket.onmessage = receiveEvent _
    leonSocket.onclose   = closeEvent _
    leonSocket.onerror   = errorEvent _
  }

  var lastChange = 0.0;
  var lastSavedChange = lastChange;
  val timeWindow = 2000;

  def updateSaveButton(): Unit = {
    val e = $("#button-save")
    if (lastChange == lastSavedChange) {
      e.addClass("disabled");
    } else {
      e.removeClass("disabled");
    }
  }

  def notify(content: String, _type: String, fade: Double = 3000): Unit = {
    val `type` = if (_type == "error") "danger" else _type

    val note = $("<div>", l(
      `class` = "alert fade in alert-" + `type`)).html("""<button type="button" class="close" data-dismiss="alert">×</button>""" + content)

    $("#notifications").append(note);

    js.timers.setTimeout(fade) {
      note.hide();
    }
  }
  Handlers += { case data: HNotification => Main.notify(data.content, data.`type`) }

  private
  var repoState = Option.empty[RepositoryState]

  def getRepositoryState: Option[RepositoryState] =
    repoState

  def modifyRepositoryState(f: RepositoryState => RepositoryState): Unit = {
    setRepositoryState(getRepositoryState.map(f))
  }

  def setRepositoryState(state: Option[RepositoryState]): Unit = {
    if (state =!= repoState) {
      state match {
        case None    => showExamples()
        case Some(_) => hideExamples()
      }

      val prevState = repoState
      val fileChanged = state.map(_.file) =!= prevState.map(_.file)

      repoState = state

      state.flatMap(_.code) match {
        case Some(code) => setEditorCode(code, fileChanged, force = true)
        case None       => recompile(force = true)
      }
    }
  }

  def setTreatAsProject(treatAsProject: Boolean): Unit = {
    modifyRepositoryState(_.copy(asProject = treatAsProject))
    recompile(force = true)
  }

  def hideExamples(): Unit = $("#selectcolumn").hide()
  def showExamples(): Unit = $("#selectcolumn").show()

  var oldCode = ""

  def recompile(force: Boolean = false) = {
    val currentCode = editor.getValue()

    if (oldCode =!= "" && oldCode =!= currentCode) {
      if (forwardChanges.length == 0) {
        storeCurrent(oldCode)
      }
    }

    if (connected && (oldCode =!= currentCode || force)) {
      oldCode         = currentCode
      lastSavedChange = lastChange

      updateSaveButton()

      repoState match {
        case Some(state) =>
          Backend.repository.doUpdateCodeInRepository(currentCode, state)

        case None =>
          Backend.main.doUpdateCode(currentCode)
      }

      Actions dispatch react.UpdateEditorCode(currentCode, updateEditor = false)

      updateCompilationStatus("unknown")
      updateCompilationProgress(0)
    }
  }

  def onCodeUpdate(): Unit = {

    val now = new js.Date().getTime()

    if (lastChange < (now - timeWindow)) {
      if (lastChange > 0) {
        recompile()
      }
      lastChange = new js.Date().getTime();
    }

    leonEditorCode := editor.getValue()
    ClarificationBox.removeSolutionButtons()
  }

  def loadSelectedExample(): Unit = {
    val selected = $("""#example-loader""").find(":selected[id]")

    val id = selected.attr("id").getOrElse("")
    val group = selected.attr("group").getOrElse("")

    loadExample(group, id)
  }

  def setEditorCode(code: String, resetEditor: Boolean = true, force: Boolean = false): Unit = {
    storeCurrent(editorSession.getValue())
    if(resetEditor) {
    editor.setValue(code);
    editor.selection.clearSelection();
    editor.gotoLine(0);
    }
    recompile(force);
  }
  
  @ScalaJSDefined
  trait StatusCode extends js.Any {
    val status: String
    val code: String
  }

  def loadExample(group: String, id: js.UndefOr[String]): Unit = {
    if (id.isDefined) {
      $.ajax(l(
        url = "/ajax/getExample/" + group + "/" + id.get,
        dataType = "json",
        success = (data: StatusCode, textStatus: String, jqXHR: JQueryXHR) => {
          if (data.status == "success") {
            setEditorCode(data.code)
            $("#example-loader").get(0).selectedIndex = 0;
          } else {
            notify("Loading example failed :(", "error")
          }
        },
        error = (jqXHR: JQueryXHR, textStatus: String, errorThrown: js.Dynamic) => {
          notify("Loading example failed :(", "error")
        }).asInstanceOf[JQueryAjaxSettings]);
    }
  }

  $("#example-loader").change(loadSelectedExample _);

  val editorSession = editor.getSession();

  val saveCommand = l(
    name = "save",
    bindKey = l(win = "Ctrl-S", mac = "Command-S").asInstanceOf[js.Any],
    exec = (((editor: Editor) => {
      recompile()
    }) :js.Function),
    readOnly = true
  ).asInstanceOf[EditorCommand]
  editor.commands.addCommand(saveCommand);

  editor.commands.removeCommand("replace");
  editor.commands.removeCommand("transposeletters");
  editor.commands.removeCommand("gotoline")

  def unsetOnChangeCallbackForEditor() = {
    EditorOnChangeCallback.enabled = false
  }

  def setOnChangeCallbackForEditor() = {
    EditorOnChangeCallback.enabled = true
  }
  object EditorOnChangeCallback {
    var enabled = true
    def callback() = {
      if(enabled){
    lastChange = new js.Date().getTime();
    updateSaveButton();
    js.timers.setTimeout(timeWindow + 50) { onCodeUpdate }
    ().asInstanceOf[js.Any]
      }
      else{
        println("Currently no callback onChange for the editor")
      }
    }
  }

  editorSession.on("change", (e: js.Any) => {
    EditorOnChangeCallback.callback()
  })
  val callbackDecorations = (e: js.Any) => {
    //println("New value: " + e + " and first visible row is " + editor.getFirstVisibleRow())
    js.timers.setTimeout(50){updateCustomGutterDecorations()}
    ().asInstanceOf[js.Any]
  }
  editorSession.on("changeFold", callbackDecorations)
  editorSession.on("changeScrollTop", callbackDecorations)
  editorSession.on("changeWrapMode", callbackDecorations)

  def resizeEditor(): Unit = {
    val h = $(window).height() - $("#title").height() - 6
    val ah = $("#annotations").height()
    val w = $("#codecolumn").width()

    $("#codecolumn").height(h);
    $("#panelscolumn").height(h);
    $("#leoninput").height(h - ah).width(w);
    $("#codebox").height(h - ah).width(w);

    editor.resize();
  };

  $(window).resize(resizeEditor _);

  resizeEditor();

  var currentMousePos = l(x = -1, y = -1);

  $(document).mousemove((event: JQueryEventObject) => {
    currentMousePos = l(x = event.pageX, y = event.pageY);
  }.asInstanceOf[js.Any]);

  def openVerifyDialog(): Unit = {
    $("#verifyDialog").modal("show")
  }
  
  def openInvariantDialog(): Unit = {
    $("#invariantDialog").modal("show")
  }

  def openTerminationDialog(): Unit = {
    $("#terminationDialog").modal("show")
  }

  val leonEditorCode = Persistent("leonEditorCode", "")
  
  var storedCode = leonEditorCode.get
  
  var onSynthesisTabDisplay: Option[() => Unit] = None
  
  storedCode foreach { code =>
    editor.setValue(code);
    editor.selection.clearSelection();
    editor.gotoLine(0);
  }
  
  Handlers += { case data: HLog =>
    val txt = $("#console")
    txt.append(data.message + "\n");
    txt.scrollTop((txt(0).scrollHeight - txt.height()).toInt)
  }
  Handlers += { case SubmitSourceCodeResult(SourceCodeSubmissionResult(optWebpage, log), javascript, requestId) =>
    optWebpage match {
      case Some(webPage) =>
        if(requestId == Backend.main.requestId) {
          websitebuilder.ScalaJS_Main.renderWebPage(webPage)
          javascript foreach websitebuilder.ScalaJS_Main.loadJavascript
        } else {
          println("Expecting id " + Backend.main.requestId + ", got " + requestId + ". Request ignored")
        }
      case None =>
    }
  }


  /*
  snowStorm.snowColor = "#ddddff";
  snowStorm.vMaxX = 2;
  snowStorm.vMaxY = 2;
  snowStorm.useTwinkleEffect = false;
  snowStorm.flakesMinActive = 350;
  snowStorm.flakesMaxActive = 350;
  snowStorm.followMouse = false;
  snowStorm.stop();
  */

}

  
