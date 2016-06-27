package leon.web.websitebuilder.clarification

import leon.web.websitebuilder.state.{SourceCodeAndProgramBinding, SourceCodeAndProgram_StronglyBinded, WBStateData}
import main.scala.leon.web.shared.webBuilding.WebElementID

/**
  * Created by dupriez on 27/06/16.
  */

/**
  *
  * @param originalSourceCodeAndProgram_stronglyBinded
  * @param idOfToBeClarifiedTextElement
  * @param potentialNewStateData Alternatives to the StateData of the State this ClarificationState is in
  * @param localStringEquations
  * @param idsOfInvolvedTextElements The ids of the textElements whose text is made out of at least one StringLiteral that appears in one of the equations
  */
class ClarificationState(
                          val originalSourceCodeAndProgram_stronglyBinded: SourceCodeAndProgram_StronglyBinded,
                          val idOfToBeClarifiedTextElement: WebElementID,
                          val potentialNewStateData: List[WBStateData],
                          val localStringEquations: List[LocalStringEquation],
                          val idsOfInvolvedTextElements: List[WebElementID]
                        ) {

}
