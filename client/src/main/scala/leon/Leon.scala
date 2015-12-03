package leon.web
package client

import scala.language.reflectiveCalls

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.JSON
import org.scalajs.dom
import org.scalajs.dom.{alert, console, document}
import org.scalajs.dom.html.Element
import scala.scalajs.js.Dynamic.{ global => g, literal => l, newInstance => jsnew }

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{ HashMap => MMap }

import japgolly.scalajs.react._

import com.scalawarrior.scalajs.ace._

import org.scalajs.jquery
import jquery.{ jQuery => $, JQueryAjaxSettings, JQueryXHR, JQuery, JQueryEventObject }
import JQueryExtended._

import Bool._
import Implicits._

import leon.web.shared.{VerifStatus, TerminationStatus, InvariantStatus}
import leon.web.shared.{Module => ModuleName, Constants, Action}
import leon.web.shared.Project

import leon.web.client.react.{App => ReactApp}

@ScalaJSDefined
class ExplorationFact(val range: Range, val res: String) extends js.Object

object ExplorationFact {
  def apply(range: Range, res: String): ExplorationFact = new ExplorationFact(range, res)
}

@ScalaJSDefined
class Feature(_a: Boolean, _n: String) extends js.Object {
  var active: Boolean = _a
  val name: String = _n
}
object Feature { def apply(active: Boolean, name: String) = new Feature(active, name) }

@JSExport
object MainDelayed extends js.JSApp {
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

@ScalaJSDefined
trait LeonSocket extends js.Object {
  def send(message: String): Unit
  var onopen: js.Function1[JQueryEventObject, Any]
  var onmessage: js.Function1[JQueryEventObject, Any]
  var onclose: js.Function1[JQueryEventObject, Any]
  var onerror: js.Function1[JQueryEventObject, Any]
}

trait LeonAPI {
  def leonSocket: LeonSocket
  def setEditorCode(code: String): Unit
  def setCurrentProject(project: Option[Project]): Unit
  def handlers: js.Dictionary[Any]
}

@JSExport("Main")
object Main extends LeonWeb with LeonAPI {

  def main(): Unit = {
    js.timers.setInterval(2000) { checkDisconnectStatus() };

    connectWS()

    val reactApp = new ReactApp(this)
    reactApp.init()

    js.timers.setTimeout(3000) {
      if (!connected) {
        $("#disconnectError").hide();
        $("#connectError").show().alert();
      }
    }

  }

}

trait LeonWeb {

  def window = g
  val editor = MainDelayed.editor
  val aceRange = ace.require("ace/range").Range;

  @ScalaJSDefined
  trait LocalStorage extends js.Any {
    def getItem(name: String): String
    def setItem(name: String, value: String): Unit
  }

  def localStorage = window.localStorage.asInstanceOf[LocalStorage]

  val hash = window.location.hash.asInstanceOf[js.UndefOr[String]]

  @JSExport val WS = !js.isUndefined(g.MozWebSocket) ? g.MozWebSocket | g.WebSocket

  @JSExport("leonSocket") var leonSocket: LeonSocket = null

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

      $(".leon-explore-location.ace_start").each((index: js.Any, _this: dom.Element) =>
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

  @ScalaJSDefined object Features extends js.Object {
    val verification=   Feature(active= true, name= "Verification")
    val synthesis=      Feature(active= true, name= "Synthesis")
    val termination=    Feature(active= false, name= "Termination <i class=\"fa fa-lightbulb-o\" title=\"Beta version\"></i>")
    val presentation=   Feature(active= false, name= "Presentation Mode")
    val execution=      Feature(active= true, name= "Execution")
    val repair=         Feature(active= true, name= "Repair <i class=\"fa fa-lightbulb-o\" title=\"Beta version\"></i>")
    val invariant=      Feature(active= true, name="Invariant inference<i class=\"fa fa-lightbulb-o\" title=\"Beta version\"></i>")
 }

  def features = Features.asInstanceOf[js.Dictionary[Feature]]

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

  $(".menu-button").click(((self: Element, event: JQueryEventObject) => {
    val target = $(self).attr("ref")
    val sel = "#" + target

    if ($(sel).is(":visible")) {
      $(sel).hide()
      $(self).addClass("disabled")
    } else {
      $(sel).show()
      $(self).removeClass("disabled")
    }

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
  // Undo/Redo
  val backwardChanges = JSON.parse(localStorage.getItem("backwardChanges")).asInstanceOf[js.UndefOr[js.Array[String]]].filter(_ =!= null).getOrElse(new js.Array[String])
  var forwardChanges = JSON.parse(localStorage.getItem("forwardChanges")).asInstanceOf[js.UndefOr[js.Array[String]]].filter(_ =!= null).getOrElse(new js.Array[String]())

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

    localStorage.setItem("backwardChanges", JSON.stringify(backwardChanges));
    localStorage.setItem("forwardChanges", JSON.stringify(forwardChanges));
  }

  updateUndoRedo()

  $("#button-permalink").click(((self: Element, event: JQueryEventObject) => {
    if (!$(self).hasClass("disabled")) {
      val msg = JSON.stringify(
        l(action = Action.storePermaLink, module = "main", code = editor.getValue()))
      leonSocket.send(msg)
    }
    event.preventDefault()
  }): js.ThisFunction);

  $("#button-permalink-close").click((event: JQueryEventObject) => {
    $("#permalink-value").hide()
  })

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
    } else if (status == "disconnected") {
      compilationStatus = 0

      e.addClass("failure")
      e.html("""Disconnected <i class="fa fa-unlink"></i>""")

      boxes.append("""<div class="overlay" />""")

    } else if (status == "unknown") {
      compilationStatus = 0

      e.html("""Compiling <i class="fa fa-refresh fa-spin" title="Compiling..."></i>""")
    } else {
      alert("Unknown status: " + status)
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

  val localFeatures = localStorage.getItem("leonFeatures")
  if (localFeatures =!= null) {
    val locFeatures = JSON.parse(localFeatures).asInstanceOf[js.Dictionary[Feature]]
    for ((f, locFeature) <- locFeatures) {
      features.get(f) match {
        case Some(feature) =>
          feature.active = locFeature.active
        case None =>
      }
    }
  }

  val fts = $("#params-panel ul")
  for ((f, feature) <- features) {
    fts.append("""<li><label class="checkbox"><input id="feature-"""" + f + " class=\"feature\" ref=\"" + f + "\" type=\"checkbox\"" + (feature.active ? """ checked="checked"""" | "") + ">" + feature.name + "</label></li>")
  }

  $(".feature").click(((self: Element) => {
    val f = $(self).attr("ref")
    features(f).active = !features(f).active

    val msg = JSON.stringify(
      l(action = Action.featureSet, module = "main", feature = f, active = features(f).active))
    leonSocket.send(msg)

    localStorage.setItem("leonFeatures", JSON.stringify(features));

    recompile()

    drawOverView()
    drawSynthesisOverview()
    setPresentationMode()
  }): js.ThisFunction)

  setPresentationMode()

  type ModulesMap = scala.collection.mutable.Map[String, Module]

  abstract class Module(name: String, list: ModulesMap) { self =>
    val column: String
    def html(name: String, d: HandlersTypes.Status): HandlersTypes.Html
    def missing(name: String): HandlersTypes.Html
    def handlers(): Unit
    list += name -> self
  }

  object overview {

    object modules {
      val list = scala.collection.mutable.Map[String, Module]() // Defined before all modules.

      val verification = new Module(ModuleName.verification, list) {
        val column = "Verif."
        def html(name: String, d: HandlersTypes.Status): HandlersTypes.Html = {
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
        def missing(name: String): HandlersTypes.Html = {
          "<td class=\"status verif\" fname=\"" + name + "\"><i class=\"fa fa-question\" title=\"unknown\"></i></td>"
        }
        def handlers(): Unit = {
          $("td.verif").click(((self: Element) => {
            val fname = $(self).attr("fname")
            overview.Data.verification.get(fname) match {
              case Some(d) =>
                openVerifyDialog()
                displayVerificationDetails(d.status, d.vcs)
              case None =>
                openVerifyDialog()
                displayVerificationDetails("unknown", new HandlersTypes.VCS())
            }
          }): js.ThisFunction)
        }
      }
      val termination = new Module(ModuleName.termination, list) {
        val column = "Term."
        def html(name: String, d: HandlersTypes.Status): HandlersTypes.Html = {
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
        def missing(name: String): HandlersTypes.Html = {
          "<td class=\"status termin\" fname=\"" + name + "\"><i class=\"fa fa-question\" title=\"unknown\"></i></td>"
        }
        def handlers(): Unit = {
          $("td.termin").click(((self: Element) => {
            val fname = $(self).attr("fname")
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
      val invariant = new Module(ModuleName.invariant, list) {
        val column = "Inv."
        def html(name: String, d: HandlersTypes.Status): HandlersTypes.Html = {
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
        def missing(name: String): HandlersTypes.Html = {
          "<td class=\"status invart\" fname=\"" + name + "\"><i class=\"fa fa-question\" title=\"unknown\"></i></td>"
        }
        def handlers(): Unit = {
          $("td.invart").click(((self: Element) => {
            val fname = $(self).attr("fname")
            overview.Data.invariant.get(fname) match {
              case Some(d) =>
                openInvariantDialog()
                displayInvariantDetails(d.status, d, overview.Data.invariant)
              case None =>
                openInvariantDialog()
                displayInvariantDetails("unknown", l().asInstanceOf[HandlersTypes.InvariantDetails], overview.Data.invariant)
            }
          }): js.ThisFunction)
        }
      }
    }

    var functions = js.Dictionary.empty[HandlersTypes.OverviewFunction]
    @ScalaJSDefined
    object Data extends js.Object {
      var verification = js.Dictionary[HandlersTypes.VerificationDetails]()
      var termination = js.Dictionary[HandlersTypes.TerminationDetails]()
      var invariant = js.Dictionary[HandlersTypes.InvariantDetails]()

      def update[A](s: String, v: A) = {
        Data.asInstanceOf[js.Dictionary[A]](s) = v
      }

      @JSName("apply")
      def apply[A](s: String): js.Dictionary[A] = {
        Data.asInstanceOf[js.Dictionary[js.Any]].get(s) match {
          case Some(dict) => dict.asInstanceOf[js.Dictionary[A]]
          case _          => throw new Exception(s"$s data not defined")
        }
      }
    }
  }

  @ScalaJSDefined trait SP extends js.Object { val index: Int; val line: Int; val description: String }

  @ScalaJSDefined trait SynthesisOverview extends js.Object {
    val functions: js.UndefOr[js.Dictionary[js.Array[SP]]]
  }

  var synthesisOverview: SynthesisOverview = new SynthesisOverview {
    val functions = js.undefined
  }

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

    val fnames = new js.Array[String]
    if (data.functions.isDefined) {
      val ff = data.functions.get
      for (f <- js.Object.keys(ff.asInstanceOf[js.Object])) {
        fnames.push(f)
      }
    }
    fnames.sort()

    for (fi <- 0 until fnames.length) {
      val f = fnames(fi);
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
      $("#synthesis .dropdown-toggle").click(((self: Element, e: JQueryEventObject) => {
        val p = $(self).parents(".problem")

        val msg = JSON.stringify(l(
          module = "synthesis",
          action = Action.getRulesToApply,
          fname = p.attr("fname"),
          cid = p.attr("cid").orIfNull("0").toInt))

        leonSocket.send(msg)
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

  def drawOverView(): Unit = {
    val overview_table = $("#overview_table")
    var html: HandlersTypes.Html = "";

    html += "<tr>"
    html += "<th>Function</th>"
    for ((name, module) <- overview.modules.list) {
      if (features(name).active) {
        html += "<th>" + module.column + "</th>"
      }
    }
    html += "</tr>"

    for ((fname, fdata) <- overview.functions) {
      val fdata = overview.functions(fname)

      html += "<tr>"
      html += "  <td class=\"fname clicktoline\" line=\"" + fdata.line + "\">" + fdata.displayName + "</td>"
      for ((m, mod) <- overview.modules.list) {
        if (features(m).active) {
          val data = overview.Data[HandlersTypes.Status](m)
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
      val line = $(_this).attr("line").toDouble
      editor.gotoLine(line);
    }): js.ThisFunction)
  }

  def addHoverToLine(within: String): Unit = {
    $("").click(((_this: Element, event: JQueryEventObject) => {
    }): js.ThisFunction)

    $(within + " .hovertoline[line]").hover((((_this: Element, event: JQueryEventObject) => {
      val line = $(_this).attr("line").toDouble
      editor.gotoLine(line).asInstanceOf[js.Any]
    }): js.ThisFunction).asInstanceOf[js.Function1[org.scalajs.jquery.JQueryEventObject, scala.scalajs.js.Any]], handlerOut = (event: JQueryEventObject) => ().asInstanceOf[js.Any])
  }

  var synthesizing = false;

  def displayVerificationDetails(status: String, vcs: HandlersTypes.VCS): Unit = {
    val pb = $("#verifyProgress")
    val pbb = pb.children(".progress-bar")

    pbb.width("100%")
    pb.removeClass("active")
    pb.addClass("progress-bar-striped")

    pbb.removeClass("progress-bar-warning progress-bar-success progress-bar-danger")

    var canRepair = false

    status match {
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

    var targetFunction: String = null
    for (i <- 0 until vcs.length) {
      val vc = vcs(i)
      targetFunction = vc.fun
      var icon = "check"
      if (vc.status == "invalid") {
        icon = "warning"
      } else if (vc.status == "unknown") {
        icon = "question"
      } else if (vc.status == "timeout") {
        icon = "clock-o"
      }

      var clas = "success"
      if (vc.status == "invalid") {
        clas = "danger"
      } else if (vc.status == "unknown" || vc.status == "timeout") {
        clas = "warning"
      }

      tbl.append("<tr class=\"" + clas + "\"> <td>" + vc.fun + "</td> <td>" + vc.kind + "</td> <td><i class=\"fa fa-" + icon + "\"></i> " + vc.status + "</td> <td>" + vc.time + "</td> </tr>")

      if (vc.counterExample.isDefined) {
        var html = "<tr class=\"" + clas + " counter-example\"><td colspan=\"4\">"
        html += "<div>"
        html += "  <p>The following inputs violate the VC:</p>";
        html += "  <table class=\"input\">";
        for((variable_name, value) <- vc.counterExample.get) { 
          html += "<tr><td>" + variable_name + "</td><td>&nbsp;:=&nbsp;</td><td>" + value + "</td></tr>";
        }
        html += "  </table>"

        if (vc.execution.isDefined && vc.execution.get.result == "success" && Features.execution.active) {
          html += "  <p>It produced the following output:</p>";
          html += "  <div class=\"output\">" + vc.execution.get.output + "</div>"
        }

        html += "    </div>"
        html += "  </td>"
        html += "</tr>"

        tbl.append(html)
      }
    }

    if (vcs.length == 0) {
      tbl.append("<tr class=\"empty\"><td colspan=\"4\"><div>No VC found</div></td></tr>")
    }

    $("div[aria-describedby='verifyDialog'] span.ui-button-text").html("Close")

    if (canRepair && Features.repair.active) {
      $(".repairButton").unbind("click").click(() => {
        val fname = targetFunction

        val msg = JSON.stringify(
          l(action = Action.doRepair, module = "repair", fname = fname))

        leonSocket.send(msg)

        $("#verifyDialog").modal("hide")
      });
      $(".repairButton").show();
    } else {
      $(".repairButton").hide();
    }

    $("#verifyResults").show("fade");
  }
  
  def displayInvariantDetails(status: String,
      invariant: HandlersTypes.InvariantDetails,
      all_invariants: js.Dictionary[HandlersTypes.InvariantDetails]): Unit = {
    
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
      Handlers.replace_code(new HandlersTypes.HReplaceCode { val newCode = invariant.newCode })
    })
    val code = all_invariants.get(Constants.invariantMainCode) match {
      case Some(result) =>
        $("#invariantDialog .importAllButton").unbind("click").click(() => {
          Handlers.replace_code(new HandlersTypes.HReplaceCode { val newCode = result.newCode })
        })
        $("#invariantDialog .importAllButton").show()
      case _ =>
        $("#invariantDialog .importAllButton").hide()
    }
    
    $("#invariantDialog").modal("show")
  }

  def displayTerminationDetails(
    status: String,
    fdata: HandlersTypes.TerminationDetails): Unit = {
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
        for (i <- 0 until fdata.calls.length) {
          html += "<tr><td>" + fdata.calls(i) + "</td></tr>";
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
    alert(msg);
  }

  val errorEvent = (event: JQueryEventObject) => {
    console.log("ERROR")
    console.log(event)
  }

  @ScalaJSDefined trait Kind extends js.Object { val kind: String }

  val receiveEvent = (event: JQueryEventObject) => {
    val data = JSON.parse(event.data.asInstanceOf[String]).asInstanceOf[Kind]
    handlers.get(data.kind) match {
      case Some(handler) =>
        handler.asInstanceOf[Function1[Kind, Any]](data);
      case _ =>
        console.log("Unknown event type: " + data.kind)
        console.log(data)
    }
  }

  var connected = false

  var lastReconnectDelay = 0;
  var reconnectIn = 0;

  val closeEvent = (event: JQueryEventObject) => {
    if (connected) {
      setDisconnected()
    }
  }

  val openEvent: JQueryEventObject => Unit = (event: JQueryEventObject) => {
    if (lastReconnectDelay =!= 0) {
      notify("And we are back online!", "success")
      updateCompilationStatus("unknown")
      oldCode = ""
    }

    setConnected()

    for ((featureName, feature) <- features) {
      val msg = JSON.stringify(
        l(action = Action.featureSet, module = "main", feature = featureName, active = feature.active))

      try {
        leonSocket.send(msg)
      }
      catch {
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
      val msg = JSON.stringify(
        l(action = Action.accessPermaLink, module = "main", link = hash.substring("#link/".length)))

      leonSocket.send(msg)
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
    updateCompilationStatus("disconnected")
    lastReconnectDelay = 5;
    reconnectIn = lastReconnectDelay;

    checkDisconnectStatus()
  }

  def setConnected(): Unit = {
    connected = true

    $("#connectError").hide();
    $("#disconnectError").hide();

    lastReconnectDelay = 0;
    reconnectIn = -1;
  }

  def checkDisconnectStatus(): Unit = {
    if (reconnectIn == 0) {
      reconnectIn = -1;
      $("#disconnectError #disconnectMsg").html("Attempting reconnection...");

      connectWS()

      // If still not connected after 2 seconds, consider failed
      js.timers.setTimeout(2000) {
        if (!connected) {
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
  def connectWS(): Unit = {
    leonSocket = jsnew(g.WebSocket /*WS*/ )(g._leon_websocket_url).asInstanceOf[LeonSocket]
    leonSocket.onopen = openEvent
    leonSocket.onmessage = receiveEvent
    leonSocket.onclose = closeEvent
    leonSocket.onerror = errorEvent
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
  var currentProject = Option.empty[Project]

  def setCurrentProject(project: Option[Project]): Unit = {
    project match {
      case None    => showExamples()
      case Some(_) => hideExamples()
    }

    currentProject = project
    recompile()
  }

  def getCurrentProject() = currentProject

  def hideExamples(): Unit = $("#selectcolumn").hide()
  def showExamples(): Unit = $("#selectcolumn").show()

  var oldCode = ""

  def recompile() = {
    val currentCode = editor.getValue()

    if (oldCode =!= "" && oldCode =!= currentCode) {
      if (forwardChanges.length == 0) {
        storeCurrent(oldCode)
      }
    }

    if (connected && oldCode =!= currentCode) {

      val msg = currentProject match {
        case Some(Project(owner, repo, branch, file)) => l(
          action = Action.doUpdateCodeInProject,
          module = "main",
          owner  = owner,
          repo   = repo,
          file   = file,
          branch = branch,
          code   = currentCode
        )

        case None => l(
          action = Action.doUpdateCode,
          module = "main",
          code   = currentCode
        )
      }

      oldCode         = currentCode;
      lastSavedChange = lastChange;

      updateSaveButton();
      leonSocket.send(JSON.stringify(msg))

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

    localStorage.setItem("leonEditorCode", editor.getValue());
  }

  def loadSelectedExample(): Unit = {
    val selected = $("""#example-loader""").find(":selected[id]")

    val id = selected.attr("id")
    val group = selected.attr("group")

    loadExample(group, id)
  }

  def setEditorCode(code: String): Unit = {
    storeCurrent(editorSession.getValue())
    editor.setValue(code);
    editor.selection.clearSelection();
    editor.gotoLine(0);
    recompile();
  }

  def loadExample(group: String, id: js.UndefOr[String]): Unit = {
    if (id.isDefined) {
      $.ajax(l(
        url = "/ajax/getExample/" + group + "/" + id.get,
        dataType = "json",
        success = (data: HandlersTypes.StatusCode, textStatus: String, jqXHR: JQueryXHR) => {
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

  editor.commands.addCommand(js.use(new js.Object {
    var name = "save"
    var bindKey = l(win = "Ctrl-S", mac = "Command-S").asInstanceOf[js.Any]
    var exec = ((editor: Editor) => {
      recompile()
    }).asInstanceOf[js.Function]
    var readOnly = true
  }).as[EditorCommand]);

  editor.commands.removeCommand("replace");
  editor.commands.removeCommand("transposeletters");

  editorSession.on("change", (e: js.Any) => {
    lastChange = new js.Date().getTime();
    updateSaveButton();
    js.timers.setTimeout(timeWindow + 50) { onCodeUpdate }
    ().asInstanceOf[js.Any]
  });

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

  var storedCode = localStorage.getItem("leonEditorCode")

  sealed class Placement(name: String) { override def toString = name }
  object Placement {
    case object Left extends Placement("left")
    case object Right extends Placement("right")
    case object Modal extends Placement("modal")
    case object Bottom extends Placement("bottom")
  }

  val seenDemo = localStorage.getItem("leonSeenDemo").orIfNull("0").toInt
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
      content = "You can generate permalinks to the editor session. If you experience any problem with the interface or if you do not understand the result, send us a link!"));

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
          action = $(self).attr("demo-action")
          hideDemo(id)
        }): js.ThisFunction)

        $("#demoPane").unbind("hide.bs.modal").on("hide.bs.modal", () => {
          if (action == "next") {
            localStorage.setItem("leonSeenDemo", (id + 1).toString)
            js.timers.setTimeout(500) { showDemo(id + 1) }
          } else {
            localStorage.setItem("leonSeenDemo", 100.toString)
          }
        })

      } else {
        var content = """<div id="demoPane" class="demo">"""
        content += demo.content
        content += """  <div class="demo-nav">"""
        if (id == demos.length - 1) {
          // last demo
          content += """    <button class="btn btn-success" demo-action="close">Ok!</button>""";
        } else {
          content += """    <button class="btn" demo-action="close">Got it</button>""";
          content += """    <button class="btn btn-success" demo-action="next">Next <i class="fa fa-forward"></i></button>""";
        }
        content += """  </div>"""
        content += """</div>"""

        val where = demo.where

        lastDemo = where;

        if (where.length == 0) {
          localStorage.setItem("leonSeenDemo", (id + 1).toString)
          hideDemo(id)
          showDemo(id + 1)
          return ;
        }

        val progress = (for (i <- 0 until (demos.length - 1)) yield {
          if (i < id) {
            """<i class="fa fa-circle"></i>"""
          } else {
            """<i class="fa fa-circle-o"></i>"""
          }
        }).mkString("")

        where.popover(l(
          html = true,
          placement = demo.placement.toString,
          trigger = "manual",
          title = """<span class="demo-progress">""" + progress + """</span>""" + demo.title,
          content = content,
          container = "body"))

        where.popover("show")

        $("#demoPane button[demo-action=\"close\"]").click(() => {
          localStorage.setItem("leonSeenDemo", 100.toString)
          hideDemo(id)
        })

        $("#demoPane button[demo-action=\"next\"]").click(() => {
          localStorage.setItem("leonSeenDemo", (id + 1).toString)
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

    storedCode = null
  }

  if (storedCode =!= null) {
    editor.setValue(storedCode);
    editor.selection.clearSelection();
    editor.gotoLine(0);
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

