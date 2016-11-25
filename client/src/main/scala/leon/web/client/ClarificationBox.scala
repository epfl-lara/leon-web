package leon.web.client
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import leon.web.client.websitebuilder.ScalaJS_Main
import leon.webDSL.webDescription.WebPageWithIDedWebElements
import org.scalajs.dom.document
import org.scalajs.jquery.{ jQuery => $, _ }

import scala.collection.mutable.ListBuffer
import scalacss.ScalaCssReact._
import leon.web.shared._

/**
  * Created by dupriez on 5/23/16.
  *
  * An object handling what is displayed in the clarification box.
  */
object ClarificationBox {
  //  val IDMang = InterfaceIDManager
  private var solutionButtonsIDList = List[String]()
  private val solutionButtonsDivID = "buttonDiv"

  def initialState = {
    //    val solutionButtonsDiv  = <.div(
    //      ^.id := solutionButtonsDiv
    //      IDMang.interfaceIDAttr := solutionButtonsDivID
    //    )
    <.div(
      ^.id := WebBuildingUIManager.clarificationBoxID,
      //      IDMang.interfaceIDAttr := "clarificationBox",
      ////      ^.id := "discussionbox",
      GlobalStyles.discussionbox,
      <.div(
        ^.id := solutionButtonsDivID
        //        IDMang.interfaceIDAttr := solutionButtonsDivID
      )

      //      <.div(
      //        IDMang.interfaceIDAttr := "discussionCommentId",
      ////        ^.id := "discussionCommentId",
      //        "Weeeeeeeelcome behind the scenes.",
      //        GlobalStyles.discussionComment,
      //        GlobalStyles.triangleBorderRight*/
      //      ),
      //      Clipart.goat()
    )
  }

  private object IDProvider {
    private var counter = 0
    def genButtonID() = {counter+=1; "clarificationBoxButtonID"+counter}
  }

  def removeSolutionButtons() = {
    val el = document.getElementById(solutionButtonsDivID)
    if (el != null)
      ReactDOM.render(<.div(), document.getElementById(solutionButtonsDivID))
  }

  def setSolutionButtons(solutions: List[ShippableClarificationSolution], idOfClarifiedWebElement: Int): Unit = {
    val clarifiedElement = $("span[data-reservedattributeforimplicitwebprogrammingid="+idOfClarifiedWebElement+"]")
    def solutionToCallback(solution: ShippableClarificationSolution): () => Unit = {
      () => {
        clarifiedElement.empty().append(solution.textContentOfClarifiedWebElementOption.getOrElse(""))
//            ScalaJS_Main.AceEditor.setEditorValue(solution.sourceCode)
        ScalaJS_Main.renderWebPage(solution.idedWebPage)
        ScalaJS_Main.submitStringModification(StringModification(idOfClarifiedWebElement, None, solution.textContentOfClarifiedWebElementOption.get))
        removeSolutionButtons()
      }
    }
    import websitebuilder.DiffViewer.FullDiff
    def solutionToOnMouseEnter(solution: ShippableClarificationSolution)(f: FullDiff): () => Unit = {
      () => clarifiedElement.empty().append(f.prefix).append(f.middle).append(f.suffix)
    }
    def solutionToOnMouseLeave(solution: ShippableClarificationSolution)(f: FullDiff): () => Unit = {
      () => clarifiedElement.empty().append(f.original)
    }
    
    val allButtons = websitebuilder.DiffViewer.displayDiffs(
        solutions.head.textContentOfClarifiedWebElementOption.getOrElse(""),
        solutions.tail.map(_.textContentOfClarifiedWebElementOption.getOrElse("")),
        15,
        GlobalStyles.webbuilderClarificationAddition.className.value,
        //GlobalStyles.webbuilderClarificationDeletion.className.value,
        GlobalStyles.webbuilderClarificationAddition.className.value,
        GlobalStyles.webbuilderClarificationModification.className.value,
        solutionToCallback(solutions.head), solutions.tail.map(solutionToCallback),
        solutionToOnMouseEnter(solutions.head), solutions.tail.map(solutionToOnMouseEnter),
        solutionToOnMouseLeave(solutions.head), solutions.tail.map(solutionToOnMouseLeave)
    )
    
    val newSolutionButtonDiv = <.div(allButtons)
    ReactDOM.render(newSolutionButtonDiv, document.getElementById(solutionButtonsDivID))
  }
}
