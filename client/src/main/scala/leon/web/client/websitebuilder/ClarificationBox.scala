package main.scala.leon.web.client.websitebuilder

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import leon.web.client.websitebuilder.ScalaJS_Main
import leon.web.client.{GlobalStyles, WebBuildingUIManager, websitebuilder}
import leon.web.shared._
import main.scala.leon.web.client.websitebuilder.state.{ClientClarificationOption_Client, ClientWBStateData_Client}
import main.scala.leon.web.shared.webBuilding.WebElementID
import org.scalajs.dom.document
import org.scalajs.jquery.{jQuery => $}

import scalacss.ScalaCssReact._

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
    ReactDOM.render(<.div(), document.getElementById(solutionButtonsDivID))
  }

//  TODO: This function is just a quick solution, that unpack and repack the data to reuse the previous setSolutionButtons function.
//  TODO: The latter should eventually be rewritten to directly accept the data.
  def setSolutionButtons_(clarificationOptions: List[ClientClarificationOption_Client], idOdClarifiedWebElement: WebElementID): Unit = {
    setSolutionButtons(
      clarificationOptions.map{
        case ClientClarificationOption_Client(clientWBStateData, textOfClarifiedWebElement) =>
          clientWBStateData match {
            case ClientWBStateData_Client(idOfCorrespondingWBStateData, sourceCode, idedWebPage, positionsOfModificationsInSourceCode) =>
              ShippableClarificationSolution(sourceCode, idedWebPage, positionsOfModificationsInSourceCode, Some(textOfClarifiedWebElement))
          }
      },
      idOdClarifiedWebElement.id,
      clarificationOptions.map {
        case ClientClarificationOption_Client(clientWBStateData, _) =>
          clientWBStateData match {
            case ClientWBStateData_Client(idOfCorrespondingWBStateData, _, _, _) =>
              idOfCorrespondingWBStateData
          }
      }
    )
  }

  def setSolutionButtons(solutions: List[ShippableClarificationSolution], idOfClarifiedWebElement: Int, listOfIdsOfCorrespondingWBStateData: List[Int]): Unit = {
    val clarifiedElement = $("span[data-reservedattributeforimplicitwebprogrammingid="+idOfClarifiedWebElement+"]")
//    Second part of the couple is idOfCorrespondingWBStateData
    def solutionToCallback(solution: (ShippableClarificationSolution, Int)): () => Unit = {
      () => {
        clarifiedElement.empty().append(solution._1.textContentOfClarifiedWebElementOption.getOrElse(""))
//            ScalaJS_Main.AceEditor.setEditorValue(solution._1.sourceCode)
        ScalaJS_Main.renderWebPage(solution._1.idedWebPage)
        solution match {
          case (
            ShippableClarificationSolution(
              sourceCode,
              idedWebPage,
              positionsOfModificationsInSourceCode,
              textContentOfClarifiedWebElementOption
            ),
            idOfCorrespondingWBStateData: Int
            ) =>
            val correspondingClientWebStateData: ClientWBStateData_Client = {
              ClientWBStateData_Client(idOfCorrespondingWBStateData, sourceCode, idedWebPage, positionsOfModificationsInSourceCode)
            }
            WebBuilderClientInterface.applyClientWBStateData(correspondingClientWebStateData)
        }
        WebBuilderClientInterface.sendStringModification(
          StringModification(idOfClarifiedWebElement, None, solution._1.textContentOfClarifiedWebElementOption.get),
          Some(solution._2)
        )
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
        solutionToCallback((solutions.head, listOfIdsOfCorrespondingWBStateData.head)), solutions.tail.zip(listOfIdsOfCorrespondingWBStateData.tail).map(solutionToCallback),
        solutionToOnMouseEnter(solutions.head), solutions.tail.map(solutionToOnMouseEnter),
        solutionToOnMouseLeave(solutions.head), solutions.tail.map(solutionToOnMouseLeave)
    )
    
    val newSolutionButtonDiv = <.div(allButtons)
    ReactDOM.render(newSolutionButtonDiv, document.getElementById(solutionButtonsDivID))
  }
}
