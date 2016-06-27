package main.scala.leon.web.shared.webBuilding

import leon.web.shared.StringPositionInSourceCode
import leon.webDSL.webDescription.WebPageWithIDedWebElements

/**
  * Created by dupriez on 27/06/16.
  */

/**
  *
  * @param sourceCode
  * @param idedWebPage
  * @param positionsOfModificationsInSourceCode
  * @param textOfClarifiedWebElement
  * @param idOfCorrespondingWBStateData The stateDataID of the WBStateData on the server from which this ClarificationOption was derived
  */
case class ClientClarificationOption(
                                      sourceCode: String,
                                      idedWebPage: WebPageWithIDedWebElements,
                                      positionsOfModificationsInSourceCode: List[StringPositionInSourceCode],
                                      textOfClarifiedWebElement: String,
                                      idOfCorrespondingWBStateData: Int
                                    )
