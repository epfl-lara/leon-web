package leon.web.websitebuilder.clarification

import leon.web.websitebuilder.state._
import main.scala.leon.web.shared.webBuilding.WebElementID

/**
  * Created by dupriez on 27/06/16.
  */

/**
  *
  * @param originalSourceCodeAndProgram_stronglyBinded
  * @param idOfToBeClarifiedTextElement
  * @param clarificationOptions Alternatives to the StateData of the State this ClarificationState is in
  * @param localStringEquations
  * @param idsOfInvolvedTextElements The ids of the textElements whose text is made out of at least one StringLiteral that appears in one of the equations
  */
case class ClarificationState(
                               originalSourceCodeAndProgram_stronglyBinded: SourceCodeAndProgram_StronglyBinded,
                               idOfToBeClarifiedTextElement: WebElementID,
                               clarificationOptions: List[ClarificationOption],
                               localStringEquations: List[LocalStringEquation],
                               idsOfInvolvedTextElements: List[WebElementID]
                        ) {
  def extractClientClarificationData: ClientClarificationData_Server = {
    ClientClarificationData_Server(
      clarificationOptions.map(_.extractClientClarificationOption),
      idOfToBeClarifiedTextElement
    )
  }
}

case class ClarificationOption(
                              wBStateData: WBStateData,
                              textOfClarifiedWebElement: String
                              ) {
  def extractClientClarificationOption: ClientClarificationOption_Server = {
    ClientClarificationOption_Server(
      wBStateData.extractClientWBStateData,
      textOfClarifiedWebElement
    )
  }
}
