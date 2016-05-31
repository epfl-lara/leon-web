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

@ScalaJSDefined
class ExplorationFact(val range: Range, val res: String) extends js.Object

object ExplorationFact {
  def apply(range: Range, res: String): ExplorationFact = new ExplorationFact(range, res)
}

@ScalaJSDefined
class Feature(_a: Boolean, _n: String, _m: Either[shared.Module, String]) extends js.Object {
  var active: Boolean = _a
  val name: String = _m.fold((m: shared.Module) => m.name, (i : String) => i )
  val displayName: String = _n
  val module: Option[shared.Module] = _m.fold(a => Some(a), _ => None)
}

object FeaturesMappings {
  var stringToModule = Map[String, shared.Module]()
  var moduleToFeature = Map[shared.Module, Feature]()
  var stringToFeature = Map[String, Feature]()
}

@JSExport
object MainDelayed extends js.JSApp {
  @JSExport
  def setTimeout(i: Int) = {
    Main.Server ! VerificationTimeout(i)
  }

  @JSExport
  def main(): Unit = {
    $(document).ready(Main.main _)
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
  def setEditorCode(code: String, resetEditor: Boolean = true): Unit
  def setCurrentProject(project: Option[Project]): Unit
  def getCurrentProject(): Option[Project]
  def setTreatAsProject(value: Boolean): Unit
  def sendMessage(m: MessageToServer): Unit
  def sendBuffered(m: MessageToServer): Unit
  /** The handler returns true if it could successfully handle the message. */
  def registerMessageHandler(m: MessageFromServer => Boolean): Unit
}

@JSExport("Main")
object Main extends LeonWeb with LeonAPI  {
  
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

trait LeonWeb extends EqSyntax {
  import boopickle.Default._
  import shared.messages.MessageToServer._
  import syntax.websocket._
  
  def window = g
  val editor = MainDelayed.editor
  val aceRange = ace.require("ace/range").Range;

  val hash = window.location.hash.asInstanceOf[js.UndefOr[String]]

  @JSExport val WS = !js.isUndefined(g.MozWebSocket) ? g.MozWebSocket | g.WebSocket

  @JSExport("leonSocket") var leonSocket: WebSocket = null
  
  private def _send(msg: ByteBuffer): Unit = leonSocket.send(new TypedArrayBufferOps(msg).arrayBuffer())

  def sendMessage(msg: MessageToServer): Unit = _send(Pickle.intoBytes(msg))
  def sendBuffered(msg: MessageToServer): Unit = leonSocket.sendBuffered(msg)
    
  object Server {// Mimick actor-like properties
    def send(msg: MessageToServer): Unit = Main.sendMessage(msg)
    // Note that if this complain with a wrong number of arguments and using callbacks, it means that the return type of the function is not correct.
    def !(msg: MessageToServer): Unit = Main.sendMessage(msg)
    
    def ![T <: MessageFromServer](msg: MessageToServerExpecting[T], callback: PartialFunction[T, Unit]): Unit = {
      Handlers.callbacks += callback.asInstanceOf[PartialFunction[U forSome { type U <: MessageFromServer }, Unit]]
      Main.sendMessage(msg)
    }
  }
  

  $("#button-permalink").click(((self: Element, event: JQueryEventObject) => {
    if (!$(self).hasClass("disabled")) {
      Server ! (StorePermaLink(editor.getValue()), ({ case data => //GotPermalink //HMoveCursor
        $("#permalink-value input").value(window._leon_url + "#link/" + data.link)
        $("#permalink-value").show()
      }): PartialFunction[GotPermalink, Unit])
      event.preventDefault()
  }}): js.ThisFunction);

  $("#button-permalink-close").click((event: JQueryEventObject) => {
    $("#permalink-value").hide()
  })

  
  def registerMessageHandler(handler: MessageFromServer => Boolean): Unit =
    Handlers.registerMessageHandler(handler)
  val headerHeight = $("#title").height() + 20

  var lastRange: Range = null;
  var lastDisplayedRange: Range = null;
  var lastProcessedRange: js.UndefOr[Range] = js.undefined;

  var explorationFacts = new js.Array[ExplorationFact]();

  var displayedMarker = -1;

  def clearExplorationFacts() = {
    lastRange = null;
    lastProcessedRange = js.undefined;

    hideHighlight();

    explorationFacts = new js.Array[ExplorationFact]();
  }

  def hideHighlight() = {
    if (displayedMarker > 0) {
      editor.getSession().removeMarker(displayedMarker);

      $(".leon-explore-location.ace_start").each((index: Int, _this: dom.Element) =>
        $(_this).tooltip("destroy").asInstanceOf[js.Any]);

    }

    lastDisplayedRange = null;
    displayedMarker = -1
  }

  def showHighlight(range: Range, content: String) = {
    if (range =!= lastDisplayedRange) {
      hideHighlight()

      lastDisplayedRange = range;

      displayedMarker = editor.getSession().addMarker(range, "leon-explore-location", "text", true);

      js.timers.setTimeout(50) {
        $(".leon-explore-location.ace_start").tooltip(l(
          title = content,
          container = "#codebox",
          placement = "top",
          trigger = "manual"))
        $(".leon-explore-location.ace_start").tooltip("show");
      }
    }
  }

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
    val termination  = Feature(active= false, displayName= "Termination <i class=\"fa fa-lightbulb-o\" title=\"Beta version\"></i>", module= Termination)
    val presentation = Feature(active= false, displayName= "Presentation Mode", name= "presentation")
    val execution    = Feature(active= true, displayName= "Execution", module= Execution)
    val repair       = Feature(active= true, displayName= "Repair <i class=\"fa fa-lightbulb-o\" title=\"Beta version\"></i>", module= Repair)
    val invariant    = Feature(active= true, displayName="Invariant inference<i class=\"fa fa-lightbulb-o\" title=\"Beta version\"></i>", module= Invariant)
  }

  def displayExplorationFacts(e: JQueryEventObject = null): js.Any = {
    if (Features.execution.active && explorationFacts.length > 0) {
      val lastRange = editor.selection.getRange();

      if (!lastProcessedRange.isDefined || !lastRange.isEqual(lastProcessedRange.get)) {
        var maxScore = 0.0
        var maxRes: ExplorationFact = null

        for (r <- explorationFacts) {
          var score = 0.0;

          val cmp = lastRange.compareRange(r.range)

          val found = ((cmp >= -1) && (cmp <= 1));

          if (cmp == -1) {
            val match_s = lastRange.start
            val match_e = r.range.end
            val before_s = r.range.start
            val after_e = lastRange.end

            score = rangeScore(match_s, match_e) -
              rangeScore(before_s, match_s) -
              rangeScore(match_e, after_e);

          } else if (cmp == 0) {
            if (lastRange.containsRange(r.range)) {
              val match_s = r.range.start
              val match_e = r.range.end
              val before_s = lastRange.start
              val after_e = lastRange.end

              score = rangeScore(match_s, match_e) -
                rangeScore(before_s, match_s) -
                rangeScore(match_e, after_e);
            } else {
              val match_s = lastRange.start
              val match_e = lastRange.end
              val before_s = r.range.start
              val after_e = r.range.end

              score = rangeScore(match_s, match_e) -
                rangeScore(before_s, match_s) -
                rangeScore(match_e, after_e);
            }
          } else if (cmp == 1) {
            val match_s = r.range.start
            val match_e = lastRange.end
            val before_s = lastRange.start
            val after_e = r.range.end

            score = rangeScore(match_s, match_e) -
              rangeScore(before_s, match_s) -
              rangeScore(match_e, after_e);
          }

          if (found && (maxRes == null || maxScore < score)) {
            maxScore = score
            maxRes = r
          }
        }

        if (maxRes =!= null) {
          showHighlight(maxRes.range, maxRes.res)
        } else {
          hideHighlight();
        }
      }

      lastProcessedRange = lastRange
    }
  }

  $("#codecolumn").mouseup(displayExplorationFacts _)

  $("#codecolumn").keyup(displayExplorationFacts _)

  def togglePanel(button: String, show: Boolean): Unit = {
    val panel = "#" + $(button).attr("ref").getOrElse("")

    if (!show) {
      $(panel).hide()
      $(button).addClass("disabled")
    } else {
      $(panel).show()
      $(button).removeClass("disabled")
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

  $("#button-save").click((event: JQueryEventObject) => {
    recompile()
    event.preventDefault()
  });

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

  def hasLocalStorage(): Boolean = {
    try {
      !js.isUndefined(window.localStorage) && window.localStorage =!= null;
    } catch {
      case e: Exception =>
        false
    }
  }

  def handlers = Handlers.asInstanceOf[js.Dictionary[Any]]

  var compilationStatus = 0
  val searchFinished = false
  var context = "unknown";

  val maxHistory = 20;

  def fromStorage[A <: js.Any](key: String): Option[A] =
    LocalStorage(key)
      .flatMap(x => x.asInstanceOf[js.UndefOr[String]].toOption)
      .map(JSON.parse(_).asInstanceOf[A])

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

  /** Compilation
    */

  def updateCompilationProgress(percents: Int): Unit = {
    $("#overview .progress-bar").css("width", percents + "%");
  }

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

  val fts = $("#params-panel ul")
  for ((f, feature) <- Features.stringToFeature) {
    fts.append("""<li><label class="checkbox"><input id="feature-"""" + f + " class=\"feature\" ref=\"" + f + "\" type=\"checkbox\"" + (feature.active ? """ checked="checked"""" | "") + ">" + feature.name + "</label></li>")
  }
  
  $(".feature").click(((self: Element) => {
    val f = $(self).attr("ref").getOrElse("")
    val feature = Features.stringToFeature(f)
    feature.active = !feature.active

    feature.module match {
      case Some(module) =>
        Backend.main.setFeatureActive(module, feature.active)
      case None =>
    }

    LocalStorage.update("leonFeatures", JSON.stringify(Features.toJsObject));

    recompile()

    drawOverView()
    drawSynthesisOverview()
    setPresentationMode()
  }): js.ThisFunction)

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
  
  var synthesisOverview: SynthesisOverview = SynthesisOverview(None)

  def drawSynthesisOverview(): Unit = {
    val t = $("#synthesis_table")
    var html = "";

    def addMenu(index: Int, fname: String, description: String): Unit = {
      val id = """menu""" + fname + index

      html += """ <div class="dropdown">"""
      html += """  <a id="""" + id + """" href="#" role="button" class="dropdown-toggle" data-toggle="dropdown"> <i class="fa fa-magic"></i> """ + description + """</a>"""
      html += """  <ul class="dropdown-menu" role="menu" aria-labelledby="""" + id + """">"""
      if (compilationStatus == 1) {
        html += """    <li role="presentation"><a role="menuitem" tabindex="-1" href="#" action="search" cid="""" + index + """">Search</a></li>"""
        html += """    <li role="presentation"><a role="menuitem" tabindex="-1" href="#" action="explore" cid="""" + index + """">Explore</a></li>"""
        html += """    <li role="presentation" class="divider"></li>"""
        html += """    <li role="presentation" class="disabled loader temp"><a role="menuitem" tabindex="-1"><img src="/assets/images/loader.gif" /></a></li>"""
      } else {
        html += """    <li role="presentation" class="disabled loader temp"><a role="menuitem" tabindex="-1"><i class="fa fa-ban"></i> Not compiled</a></li>"""
      }

      html += """  </ul>"""
      html += """ </div>"""
    }

    val data = synthesisOverview

    val fnamesOpt: Option[Array[String]] = data.functions.map(ff => ff.unzip._1.toArray)
    val fnames = fnamesOpt.getOrElse[Array[String]](Array[String]()).sorted

    for (f <- fnames) {
      overview.functions.get(f) match {
        case Some(function) =>
          if (data.functions.get(f).length == 1) {
            val sp = data.functions.get(f)(0)
            html += "<tr><td class=\"fname problem  clicktoline\" line=\"" + sp.line + "\" fname=\"" + f + "\" cid=\"" + sp.index + "\">"
            addMenu(sp.index, f, function.displayName)
            html += "</td></tr>"
          } else {
            html += "<tr><td class=\"fname clicktoline\" line=\"" + function.line + "\">" + function.displayName + "</td></tr>"
            val spArray = data.functions.get(f)
            for (i <- 0 until spArray.length) {
              val sp = spArray(i)
              html += "<tr>"
              html += "<td class=\"problem subproblem clicktoline\" line=\"" + sp.line + "\" fname=\"" + f + "\" cid=\"" + sp.index + "\">"
              addMenu(sp.index, f, sp.description)
              html += "</td></tr>"
            }
          }
        case None =>
      }
    }

    t.html(html);

    if (compilationStatus == 1) {
      $("#synthesis .dropdown-toggle").unbind("click.droppdown").on("click.droppdown", ((self: Element, e: JQueryEventObject) => {
        val p = $(self).parents(".problem")

        Backend.synthesis.getRulesToApply(p.attr("fname").getOrElse("0"), p.attr("cid").getOrElse("0").toInt)

      }): js.ThisFunction)
    }
    
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
      Handlers.replace_code(HReplaceCode(newCode = invariant.newCode))
    })
    val code = all_invariants.get(Constants.invariantMainCode) match {
      case Some(result) =>
        $("#invariantDialog .importAllButton").unbind("click").click(() => {
          Handlers.replace_code(HReplaceCode(newCode = result.newCode))
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

  private
  var treatAsProject = true

  def setTreatAsProject(value: Boolean): Unit = {
    treatAsProject = value
    recompile(force = true)
  }

  private
  var currentProject = Option.empty[Project]

  def getCurrentProject(): Option[Project] =
    currentProject

  def setCurrentProject(project: Option[Project]): Unit = {
    if (project =!= currentProject) {
    project match {
      case None    => showExamples()
      case Some(_) => hideExamples()
    }
    currentProject = project
      project.flatMap(_.code).foreach(setEditorCode(_, project.map(_.file) != currentProject.map(_.file)))
  
    recompile(force = true)
  }
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
      currentProject match {
        case Some(Project(owner, repo, branch, file, _)) if treatAsProject =>
          Backend.main.doUpdateCodeInProject(
            owner  = owner,
            repo   = repo,
            file   = file,
            branch = branch,
            code   = currentCode
          )

        case _ =>
          Backend.main.doUpdateCode(
            code   = currentCode
          )
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

    LocalStorage.update("leonEditorCode", editor.getValue());
  }

  def loadSelectedExample(): Unit = {
    val selected = $("""#example-loader""").find(":selected[id]")

    val id = selected.attr("id").getOrElse("")
    val group = selected.attr("group").getOrElse("")

    loadExample(group, id)
  }

  def setEditorCode(code: String, resetEditor: Boolean = true): Unit = {
    storeCurrent(editorSession.getValue())
    if(resetEditor) {
    editor.setValue(code);
    editor.selection.clearSelection();
    editor.gotoLine(0);
    }
    recompile();
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
  
  editorSession.on("change", (e: js.Any) => {
    lastChange = new js.Date().getTime();
    updateSaveButton();
    js.timers.setTimeout(timeWindow + 50) { onCodeUpdate }
    ().asInstanceOf[js.Any]
  });
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

  var storedCode = LocalStorage("leonEditorCode")
  
  var onSynthesisTabDisplay: Option[() => Unit] = None

  sealed class Placement(name: String) { override def toString = name }
  object Placement {
    case object Left extends Placement("left")
    case object Right extends Placement("right")
    case object Modal extends Placement("modal")
    case object Bottom extends Placement("bottom")
    case object Top extends Placement("top")
  }

  val seenDemo = LocalStorage("leonSeenDemo").getOrElse("0").toInt
  @ScalaJSDefined class Demo(_where: => JQuery, _title: String, _content: String, _placement: Placement) extends js.Object {
    def where: JQuery = _where
    val title: String = _title
    val content: String = _content
    val placement: Placement = _placement
  }
  object Demo {
    def apply(where: => JQuery, title: String, content: String, placement: Placement): Demo = new Demo(where, title, content, placement)
  }

  val demos = js.Array[Demo](
    Demo(
      where = $(""),
      placement = Placement.Modal,
      title = "Welcome to Leon!",
      content = "Leon is an automated system for <strong>synthesizing</strong> and <strong>verifying</strong> functional Scala programs."),
    Demo(
      where = $("#example-loader"),
      placement = Placement.Left,
      title = "Select from examples",
      content = "You can try <em>Leon</em> on a list of selected examples, covering both synthesis and verification problems."),
    Demo(
      where = $($(".ace_line_group")(13)).find("span").last(),
      placement = Placement.Right,
      title = "Edit at will",
      content = "Feel free to modify or extend the selected examples with your own code."),
    Demo(
      where = $("#overview_table"),
      placement = Placement.Left,
      title = "Live results",
      content = "Leon will verify your code in the background and display live verification results here."),
    Demo(
      where = $($("#overview_table td.status.verif")(2)),
      placement = Placement.Left,
      title = "Display details",
      content = "Click on the verification status of each function to get more information!"),
    Demo(
      where = $("#synthesis_table td.problem").first(),
      placement = Placement.Left,
      title = "Synthesize",
      content = "Click on a synthesis problem to solve it! You can either ask <em>Leon</em> to <strong>search</strong> for a solution, or perform individual steps yourself."),
    Demo(
      where = $("#button-permalink"),
      placement = Placement.Bottom,
      title = "Permalinks",
      content = "You can generate permalinks to the editor session. If you experience any problem with the interface or if you do not understand the result, send us a link!")
  );
  
  def getHtmlDemo(demo: Demo, last: Boolean): String = {
    var content = """<div id="demoPane" class="demo">"""
    content += demo.content
    content += """  <div class="demo-nav">"""
    if (last) {
      // last demo
      content += """    <button class="btn btn-success" demo-action="close">Ok!</button>""";
    } else {
      content += """    <button class="btn" demo-action="close">Got it</button>""";
      content += """    <button class="btn btn-success" demo-action="next">Next <i class="fa fa-forward"></i></button>""";
    }
    content += """  </div>"""
    content += """</div>"""
    content
  }
       
  /** Creates a progress bar with "current" filled circles out of "max" circles. */
  def createProgress(current: Int, max: Int): String = {
    (for (i <- 0 until max) yield {
      if (i < current) {
        """<i class="fa fa-circle"></i>"""
      } else {
        """<i class="fa fa-circle-o"></i>"""
      }
    }).mkString("")
  }

  if (seenDemo == 0 || (seenDemo < demos.length - 1)) {

    var lastDemo: JQuery = null // Do we have something better?

    def showDemo(id: Int): Unit = {
      val demo = demos(id)

      if (demo.placement == Placement.Modal) {
        // Assume only the first demo is modal
        var html = """<div id="demoPane" class="modal fade" tabindex="-1" role="dialog" aria-labelledby="demoModal" aria-hidden="true" data-backdrop="static">"""
        html += """  <div class="modal-dialog">"""
        html += """    <div class="modal-content">"""
        html += """      <div class="modal-header">"""
        html += """        <button type="button" class="close" demo-action="close" data-dismiss="modal" aria-hidden="true">×</button>"""
        html += """        <h3 id="demoModal">""" + demo.title + """</h3>"""
        html += """      </div>"""
        html += """      <div class="modal-body">"""
        html += """        """ + demo.content
        html += """      </div>"""
        html += """      <div class="modal-footer">"""
        html += """        <button class="btn btn-success" data-dismiss="modal" aria-hidden="true" demo-action="next">Take the tour <i class="fa fa-play"></i></button>"""
        html += """        <button class="btn" data-dismiss="modal" aria-hidden="true" demo-action="close">No thanks</button>"""
        html += """      </div>"""
        html += """    </div>"""
        html += """  </div>"""
        html += """</div>"""

        $("body").append(html);

        $("#demoPane").modal("show")

        var action = "close"

        $("#demoPane button").click(((self: Element) => {
          action = $(self).attr("demo-action").getOrElse("")
          hideDemo(id)
        }): js.ThisFunction)

        $("#demoPane").unbind("hide.bs.modal").on("hide.bs.modal", () => {
          if (action == "next") {
            LocalStorage.update("leonSeenDemo", (id + 1).toString)
            js.timers.setTimeout(500) { showDemo(id + 1) }
          } else {
            LocalStorage.update("leonSeenDemo", 100.toString)
          }
        })

      } else {
        lastDemo = demo.where;
        
        val where = demo.where

        if (where.length == 0) {
          LocalStorage.update("leonSeenDemo", (id + 1).toString)
          hideDemo(id)
          showDemo(id + 1)
          return ;
        }

        val progress = createProgress(id, demos.length - 1)

        where.popover(l(
          html = true,
          placement = demo.placement.toString,
          trigger = "manual",
          title = """<span class="demo-progress">""" + progress + """</span>""" + demo.title,
          content = getHtmlDemo(demo, id == demos.length - 1),
          container = "body"))

        where.popover("show")

        $("#demoPane button[demo-action=\"close\"]").click(() => {
          LocalStorage.update("leonSeenDemo", 100.toString)
          hideDemo(id)
        })

        $("#demoPane button[demo-action=\"next\"]").click(() => {
          LocalStorage.update("leonSeenDemo", (id + 1).toString)
          hideDemo(id)
          showDemo(id + 1)
        })
      }
    }

    def hideDemo(id: Int): Unit = {
      val demo = demos(id)

      if (demo.placement == Placement.Modal) {
        $("#demoPane").modal("hide")
        $("#demoPane").unbind("hidden").on("hidden", () => { $("demoPane").remove() })
      } else {
        lastDemo.popover("destroy")
      }
    }

    val toShow = (seenDemo =!= 0) ? seenDemo | 0;
    if (toShow =!= 0) {
      js.timers.setTimeout(1000) { showDemo(toShow) }
    } else {
      showDemo(toShow)
    }

    storedCode = None
  }

  storedCode foreach { code =>
    editor.setValue(code);
    editor.selection.clearSelection();
    editor.gotoLine(0);
  }
  
  /* Demos to be shown in context of some events. */
  val demoClarificationMenu =
    Demo(
      where = $("a[href=#clarificationResults]"),
      placement = Placement.Top,
      title = "Clarification tab",
      content = """Although the solution works for the given examples, you may want to investigate ambiguities and resolve them."""
    )
  
  val demoClarification =
    Demo(
      where = $("#clarificationResults"),
      placement = Placement.Top,
      title = "Clarification",
      content = """The clarification tab shows ambiguous outputs produced by custom examples.<br>You may edit them - especially to replace the <span class="placeholder" style="font-family:FontAwesome">&#xf059;</span> markers. If you edit them, remember to validate them."""
    )

  val demoEditCounterExamples =
    Demo(
      where = $("#verifyResults"),
      placement = Placement.Top,
      title = "Edit counter examples",
      content = """You are looking at counter-examples which look complex.<br>If you wish, you can edit how they should be rendered by clicking on one of them, editing it and clicking on "validate"."""
    )
    
  val demoSynthesizePrettyPrinter =
    Demo(
      where = $("#synthesis_table td.problem").first(),
      placement = Placement.Left,
      title = "Synthesize Pretty Printer",
      content = """After showing how to render the counter-example, click to synthesize the renderer. After doing so and having clicked on "import code", the pretty printer will be ready."""
    )
    
  def showContextDemo(demo: Demo, demos: Demo*): Unit = showContextDemo(demo +: demos.toSeq, 0)
    
  def showContextDemo(demos: Seq[Demo], index: Int): Unit = {
    if(demos.length == 0 || index >= demos.length) return;
    val nth = index + 1
    val maxNth = demos.length
    val demo = demos(index)
    def storageName(demo: Demo) = "leonSeenDemo_"+demo.title
    def next() = showContextDemo(demos, index + 1)

    val seenDemo = LocalStorage(storageName(demo)).getOrElse("") == "closed"
    if(seenDemo) return next();
    
    val content = getHtmlDemo(demo, last = true)
    val where = demo.where
    
    val progress = createProgress(nth, maxNth)
    where.popover(l(
      html = true,
      placement = demo.placement.toString,
      trigger = "manual",
      title = """<span class="demo-progress">""" + progress + """</span>""" + demo.title,
      content = getHtmlDemo(demo, nth == maxNth),
      container = "body"))

    where.popover("show")
    
    $("#demoPane button[demo-action=\"close\"]").click(() => {
      LocalStorage.update(storageName(demo), "closed")
      where.popover("destroy")
    })

    $("#demoPane button[demo-action=\"next\"]").click(() => {
      LocalStorage.update(storageName(demo), "closed")
      where.popover("destroy")
      next()
    })
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

