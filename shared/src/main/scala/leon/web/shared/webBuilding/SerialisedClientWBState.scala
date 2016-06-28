package main.scala.leon.web.shared.webBuilding

import leon.web.shared.StringPositionInSourceCode


import leon.webDSL.webDescription.WebPageWithIDedWebElements

/**
  * Created by dupriez on 27/06/16.
  */

case class SerialisedClientWBState(
                                    stateID: Int,
//                                  stateData
                                    idOfCorrespondingWBStateData: Int,
                                    sourceCode: String,
                                    idedWebPage: WebPageWithIDedWebElements,
                                    positionsOfModificationsInSourceCode: List[StringPositionInSourceCode],
//                                  clarificationData
                                    idOfToBeClarifiedWebElement: WebElementID,
//                                    clientClarificationOptions
                                      idOfCorrespondingWBStateData_list: List[Int],
                                      sourceCode_list: List[String],
                                      idedWebPage_list: List[WebPageWithIDedWebElements],
                                      positionsOfModificationsInSourceCode_list: List[List[StringPositionInSourceCode]],
                                      textOfClarifiedWebElement_list: List[String]
                                  )
