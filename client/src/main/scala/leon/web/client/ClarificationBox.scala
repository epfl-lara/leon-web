package leon.web.client
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import leon.web.client.websitebuilder.ScalaJS_Main
import leon.webDSL.webDescription.WebPageWithIDedWebElements
import org.scalajs.dom.document

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
      ^.id := "clarificationBox",
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

  /**
    * Creates a react button with the provided label and callbacks.
    * Returns a couple: (the button, it's interfaceID)
    *
    * @param label
    * @param onClickCallback
    * @param onMouseEnterCallback
    * @param onMouseLeaveCallback
    * @return
    */
  private def createButton(
                            label: String,
                            onClickCallback: ()=>Unit,
                            onMouseEnterCallback: ()=>Unit,
                            onMouseLeaveCallback: ()=>Unit
                          ) : (ReactTagOf[org.scalajs.dom.html.Button], String) = {
    val buttonID = IDProvider.genButtonID()
    (
      <.button(
        label,
        ^.id := buttonID,
        //        IDMang.interfaceIDAttr := buttonID,
        ^.className := "btn btn-default",
        //      ^.verticalAlign := "-webkit-baseline-middle",
        ^.onClick --> Callback{onClickCallback()},
        ^.onMouseEnter --> Callback{onMouseEnterCallback()},
        ^.onMouseLeave --> Callback{onMouseLeaveCallback()}
      ),
      buttonID
      )
  }

  def setSolutionButtons(solutions: List[ShippableClarificationSolution], idOfClarifiedWebElement: Int): Unit = {
    //    solutionButtonsIDList = List()
    object LabelProducer {
      private var counter = 0
      def genLabel() = {counter += 1; "Solution "+counter}
    }
    //    TODO: complete this function so that it generates buttons that do interesting things
    def createAndRegisterSolutionButton(solution: ShippableClarificationSolution, idOfClarifiedWebElement: Int) : ReactTagOf[org.scalajs.dom.html.Button] = {
      //      println(
      //        s"""Creating a solution button:
      //           |  idOfClarifiedWebElement: $idOfClarifiedWebElement
      //           |  sourceCode: ${solution.sourceCode}
      //           |  webPage: ${solution.idedWebPage}
      //           |  textContentOfCLarifiedWebElement: ${solution.textContentOfClarifiedWebElementOption.get}
      //         """.stripMargin)
      val (button, interfaceID) = createButton(
        LabelProducer.genLabel(),
        onClickCallback = {
          () => {
            ScalaJS_Main.AceEditor.setEditorValue(solution.sourceCode)
            ScalaJS_Main.renderWebPage(solution.idedWebPage)
            ScalaJS_Main.submitStringModification(StringModification(idOfClarifiedWebElement, None, solution.textContentOfClarifiedWebElementOption.get))
          }
        },
        () => println("entered"),
        () => println("left")
      )
      //      solutionButtonsIDList += interfaceID
      button
    }
    val newSolutionButtonDiv = <.div(solutions.map(solution => createAndRegisterSolutionButton(solution, idOfClarifiedWebElement)))
    ReactDOM.render(newSolutionButtonDiv, document.getElementById(solutionButtonsDivID))
  }
}
