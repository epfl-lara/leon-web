package leon.web.websitebuilder.state

import leon.web.shared.StringPositionInSourceCode
import leon.webDSL.webDescription.{WebPage, WebPageWithIDedWebElements}
import main.scala.leon.web.shared.webBuilding.SerialisedClientWBState

/**
  * Created by dupriez on 28/06/16.
  */
object ClientWBStateSerialiser {
  def serialise(clientWBState_Server: ClientWBState_Server): SerialisedClientWBState = {
    clientWBState_Server match {
      case ClientWBState_Server(stateID, stateData, clientClarificationData_option) =>
        stateData match {
          case ClientWBStateData_Server(
            idOfCorrespondingWBStateData,
            sourceCode,
            idedWebPage,
            positionsOfModificationsInSourceCode
          ) =>
            clientClarificationData_option match {
              case None =>
                SerialisedClientWBState(
                  stateID,
                  idOfCorrespondingWBStateData,
                  sourceCode,
                  idedWebPage,
                  positionsOfModificationsInSourceCode,
                  None,
                  None,
                  None,
                  None,
                  None,
                  None
                )
              case Some(ClientClarificationData_Server(clarificationOptions, idOfToBeClarifiedWebElement)) =>
                val (
                  idOfCorrespondingWBStateData_list,
                  sourceCode_list,
                  idedWebPage_list,
                  positionsOfModificationsInSourceCode_list,
                  textOfClarifiedWebElement_list
                ) = clarificationOptions.foldLeft(
                  (
                    List[Int](),
                    List[String](),
                    List[WebPageWithIDedWebElements](),
                    List[List[StringPositionInSourceCode]](),
                    List[String]()
                    )
                ){
                  case (
                        (
                          idOfCorrespondingWBStateData_list,
                          sourceCode_list,
                          idedWebPage_list,
                          positionsOfModificationsInSourceCode_list,
                          textOfClarifiedWebElement_list
                        ),
                        ClientClarificationOption_Server(
                          ClientWBStateData_Server(
                            idOfCorrespondingWBStateData,
                            sourceCode,
                            idedWebPage,
                            positionsOfModificationsInSourceCode
                          ),
                          textOfClarifiedWebElement
                        )
                      ) =>
                    (
                      idOfCorrespondingWBStateData_list :+ idOfCorrespondingWBStateData,
                      sourceCode_list :+ sourceCode,
                      idedWebPage_list :+ idedWebPage,
                      positionsOfModificationsInSourceCode_list :+ positionsOfModificationsInSourceCode,
                      textOfClarifiedWebElement_list :+ textOfClarifiedWebElement
                      )
                }
                SerialisedClientWBState(
                  stateID,
                  idOfCorrespondingWBStateData,
                  sourceCode,
                  idedWebPage,
                  positionsOfModificationsInSourceCode,
                  Some(idOfToBeClarifiedWebElement),
                  Some(idOfCorrespondingWBStateData_list),
                  Some(sourceCode_list),
                  Some(idedWebPage_list),
                  Some(positionsOfModificationsInSourceCode_list),
                  Some(textOfClarifiedWebElement_list)
                )
            }

        }
    }
  }
}
