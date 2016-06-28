package leon.web.websitebuilder.state

import leon.web.shared.StringPositionInSourceCode
import leon.web.websitebuilder.SourceMap
import leon.webDSL.webDescription.WebPageWithIDedWebElements

/**
  * Created by dupriez on 27/06/16.
  */

object WBStateData {
  private var idCounter = 0
  def generateStateDataID() = {
    idCounter += 1
    idCounter
  }
}

class WBStateData(
                   val sourceCodeAndProgramBinding: SourceCodeAndProgramBinding,
                   val idedWebPage: WebPageWithIDedWebElements,
                   val sourceMap: SourceMap,
//                   These positions are with regards to the source code of the previous state
                   val positionsOfModificationsInSourceCode_option: Option[List[StringPositionInSourceCode]]
                 ) {
  val stateDataID = WBStateData.generateStateDataID()

  def convertToClientWBStateData: ClientWBStateData_Server = {
    ???
  }
}

