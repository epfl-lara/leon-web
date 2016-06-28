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
                                    idOfToBeClarifiedWebElement_option: Option[WebElementID],
//                                    clientClarificationOptions
                                      idOfCorrespondingWBStateData_list_option: Option[List[Int]],
                                      sourceCode_list_option: Option[List[String]],
                                      idedWebPage_list_option: Option[List[WebPageWithIDedWebElements]],
                                      positionsOfModificationsInSourceCode_list_option: Option[List[List[StringPositionInSourceCode]]],
                                      textOfClarifiedWebElement_list_option: Option[List[String]]
                                  )
