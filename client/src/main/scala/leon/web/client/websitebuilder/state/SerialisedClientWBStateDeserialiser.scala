package main.scala.leon.web.client.websitebuilder.state

import leon.web.shared.StringPositionInSourceCode
import leon.webDSL.webDescription.WebPageWithIDedWebElements
import main.scala.leon.web.shared.webBuilding.{SerialisedClientWBState, WebElementID}

/**
  * Created by dupriez on 28/06/16.
  */
object SerialisedClientWBStateDeserialiser {
  def deserialise(serialisedClientWBState: SerialisedClientWBState): ClientWBState_Client = {
    serialisedClientWBState match {
      case SerialisedClientWBState(
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
      ) =>
        val mainClientWBStateData = ClientWBStateData_Client(
          idOfCorrespondingWBStateData,
          sourceCode,
          idedWebPage,
          positionsOfModificationsInSourceCode
        )
//        Check whether there is a ClarificationData in the serialised ClientWBState
        idOfToBeClarifiedWebElement_option match {
          case None =>
            ClientWBState_Client(
              stateID,
              mainClientWBStateData,
              None
            )
          case Some(idOfToBeClarifiedWebElement) =>
            val (
              idOfCorrespondingWBStateData_list,
              sourceCode_list,
              idedWebPage_list,
              positionsOfModificationsInSourceCode_list,
              textOfClarifiedWebElement_list
            ) = {
              (
              idOfCorrespondingWBStateData_list_option.get,
              sourceCode_list_option.get,
              idedWebPage_list_option.get,
              positionsOfModificationsInSourceCode_list_option.get,
              textOfClarifiedWebElement_list_option.get
                )
            }
            val clientClarificationOptions_list: List[ClientClarificationOption_Client] =
            List.range(0, idOfCorrespondingWBStateData_list.length-1).map{
              case i: Int =>
                ClientClarificationOption_Client(
                  ClientWBStateData_Client(
                    idOfCorrespondingWBStateData_list(i),
                    sourceCode_list(i),
                    idedWebPage_list(i),
                    positionsOfModificationsInSourceCode_list(i)
                  ),
                  textOfClarifiedWebElement_list(i)
                )
            }
            val clientClarificationData_Client = ClientClarificationData_Client(clientClarificationOptions_list, idOfToBeClarifiedWebElement)
            ClientWBState_Client(
              stateID,
              mainClientWBStateData,
              Some(clientClarificationData_Client)
            )
        }
    }
  }
}
