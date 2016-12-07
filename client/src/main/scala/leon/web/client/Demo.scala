package leon.web
package client

import scala.language.reflectiveCalls
import scala.scalajs.js
import scala.scalajs.js.annotation._
import org.scalajs.dom.ext.LocalStorage
import org.scalajs.jquery
import jquery.{ jQuery => $, JQuery }
import org.scalajs.dom.html.Element
import js.Dynamic.{ /*global => g, */literal => l/*, newInstance => jsnew*/ }
import JQueryExtended._
import Bool._

sealed class Placement(name: String) { override def toString = name }
object Placement {
  case object Left extends Placement("left")
  case object Right extends Placement("right")
  case object Modal extends Placement("modal")
  case object Bottom extends Placement("bottom")
  case object Top extends Placement("top")
}
  
@ScalaJSDefined class Demo(_where: () =>JQuery, _title: String, _content: String, _placement: Placement) extends js.Object {
  def where: JQuery = _where()
  val title: String = _title
  val content: String = _content
  val placement: Placement = _placement
}
object Demo {
  def apply(where: => JQuery, title: String, content: String, placement: Placement): Demo = new Demo(() => where, title, content, placement)
}

trait DemoHandler { self: Main.type =>
  val leonSeenDemo = Persistent("leonSeenDemo", 0)

  val seenDemo = leonSeenDemo: Int

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
            leonSeenDemo := id + 1
            js.timers.setTimeout(500) { showDemo(id + 1) }
          } else {
            leonSeenDemo := 100
          }
        })

      } else {
        lastDemo = demo.where;
        
        val where = demo.where

        if (where.length == 0) {
          leonSeenDemo := id + 1
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
          leonSeenDemo := 100
          hideDemo(id)
        })

        $("#demoPane button[demo-action=\"next\"]").click(() => {
          leonSeenDemo := id + 1
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
    
  val demoWebpageFullscreen =
    Demo(
      where = $("#separatewindowlink").first(),
      placement = Placement.Left,
      title = "External window",
      content = "You can view the webpage you generated in an external window"
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
  
}