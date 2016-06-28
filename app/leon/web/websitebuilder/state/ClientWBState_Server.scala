package leon.web.websitebuilder.state

import leon.web.shared.StringPositionInSourceCode
import leon.webDSL.webDescription.WebPageWithIDedWebElements
import main.scala.leon.web.shared.webBuilding.WebElementID


/**
  * Created by dupriez on 28/06/16.
  *
  * Copy-pasting of the class ClientWBState_Client, on the client, because of
  *   "Cannot materialise picklers for non case-class" errors and sbt launching Java.OutOfMemory Exception when compiling
  */


/**
  * Contains a subset of the information of a WBState, so that it can be serialised and shipped to the client.
  *
  * The String in clarificationData_option is the text of the clarified element according to the coupled ClientWBStateData
  */
case class ClientWBState_Server(stateID: Int, stateData: ClientWBStateData_Server, clientClarificationData_option: Option[ClientClarificationData_Server])

/**
  *
  * @param idOfCorrespondingWBStateData The stateDataID of the WBStateData on the server from which this ClarificationOption was derived
  * @param sourceCode
  * @param idedWebPage
  * @param positionsOfModificationsInSourceCode
  */
case class ClientWBStateData_Server(
                              idOfCorrespondingWBStateData: Int,
                              sourceCode: String,
                              idedWebPage: WebPageWithIDedWebElements,
                              positionsOfModificationsInSourceCode: List[StringPositionInSourceCode]
                            )

case class ClientClarificationData_Server(clarificationOptions: List[ClientClarificationOption_Server], idOfToBeClarifiedWebElement: WebElementID)

case class ClientClarificationOption_Server(
                                      clientWBStateData: ClientWBStateData_Server,
                                      textOfClarifiedWebElement: String
                                    )