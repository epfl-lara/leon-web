package leon.web.websitebuilder.state

import leon.web.websitebuilder.clarification.ClarificationState
import main.scala.leon.web.shared.webBuilding.ClientWBState

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

class WBState(
               val stateData: WBStateData,
               val clarificationState: ClarificationState
           ) {
  val stateID = WBState.generateStateID()

  def convertToClientWBState: ClientWBState = {
    ???
  }
}