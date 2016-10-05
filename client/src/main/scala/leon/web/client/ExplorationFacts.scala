
package leon.web
package client
import scala.scalajs.js
import js.Dynamic.{ /*global => g, */literal => l}
import js.annotation._
import org.scalajs.jquery
import jquery.{ jQuery => $, JQueryEventObject }
import com.scalawarrior.scalajs.ace.Range
import org.scalajs.dom
import JQueryExtended._

@ScalaJSDefined
class ExplorationFact(val range: Range, val res: String) extends js.Object

object ExplorationFact {
  def apply(range: Range, res: String): ExplorationFact = new ExplorationFact(range, res)
}

trait ExplorationFactHandler { self: Main.type =>
  var explorationFacts = new js.Array[ExplorationFact]();
  var lastRange: Range = null;
  
  var lastDisplayedRange: Range = null;
  var lastProcessedRange: js.UndefOr[Range] = js.undefined;

  var displayedMarker = -1;

  def hideHighlight() = {
    if (displayedMarker > 0) {
      editor.getSession().removeMarker(displayedMarker)

      $(".leon-explore-location.ace_start").each((index: Int, _this: dom.Element) =>
        $(_this).tooltip("destroy").asInstanceOf[js.Any])

    }

    lastDisplayedRange = null
    displayedMarker = -1
  }

  def showHighlight(range: Range, content: String) = {
    if (range =!= lastDisplayedRange) {
      hideHighlight()

      lastDisplayedRange = range

      displayedMarker = editor.getSession().addMarker(range, "leon-explore-location", "text", true)

      js.timers.setTimeout(50) {
        $(".leon-explore-location.ace_start").tooltip(l(
          title = content,
          container = "#codebox",
          placement = "top",
          trigger = "manual"))
        $(".leon-explore-location.ace_start").tooltip("show")
      }
    }
  }

  
  def clearExplorationFacts() = {
    lastRange = null;
    lastProcessedRange = js.undefined;

    hideHighlight();

    explorationFacts = new js.Array[ExplorationFact]();
  }
  
  
  def displayExplorationFacts(e: JQueryEventObject = null): js.Any = {
    if (Features.execution.active && explorationFacts.length > 0) {
      val lastRange = editor.selection.getRange();

      if (!lastProcessedRange.isDefined || !lastRange.isEqual(lastProcessedRange.get).asInstanceOf[Boolean]) {
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

}