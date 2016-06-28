package leon.web.websitebuilder.state

import leon.web.shared.StringPositionInSourceCode
import leon.web.websitebuilder.webPageExprConversion.SourceMap
import leon.web.websitebuilder.clarification.ClarificationState
import leon.webDSL.webDescription.WebPageWithIDedWebElements

import scala.concurrent.Future

/**
  * Created by dupriez on 27/06/16.
  */

object WBState {
  private var idCounter = 0
  def generateStateID() = {
    idCounter += 1
    idCounter
  }
}

case class WBState(
               stateData: WBStateData,
               clarificationState_option: Option[ClarificationState]
           ) {
  val stateID = WBState.generateStateID()

  def extractToClientWBState: ClientWBState_Server = {
    ClientWBState_Server(
     stateID,
      stateData.extractClientWBStateData,
      clarificationState_option.map(_.extractClientClarificationData)
    )
  }
}

object WBStateData {
  private var idCounter = 0
  def generateStateDataID() = {
    idCounter += 1
    idCounter
  }
}

case class WBStateData(
                   sourceCodeAndProgramBinding: SourceCodeAndProgramBinding,
                   idedWebPage: WebPageWithIDedWebElements,
                   sourceMap_future: Future[SourceMap],
                   //                   These positions are with regards to the source code of the previous state
                   positionsOfModificationsInSourceCode: List[StringPositionInSourceCode]
                 ) {
  val stateDataID = WBStateData.generateStateDataID()

  def extractClientWBStateData: ClientWBStateData_Server = {
    ClientWBStateData_Server(
      stateDataID,
      sourceCodeAndProgramBinding.sourceCode,
      idedWebPage,
      positionsOfModificationsInSourceCode
    )
  }
}
