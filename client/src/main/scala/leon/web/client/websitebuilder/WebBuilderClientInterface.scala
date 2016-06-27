package main.scala.leon.web.client.websitebuilder

import leon.web.client.Main.Server
import leon.web.client.websitebuilder.ScalaJS_Main
import leon.web.shared.StringModification
import leon.web.shared.messages.{NewClientWebBuildingState, SendStringModification}
import main.scala.leon.web.shared.webBuilding.{ClientClarificationData, ClientClarificationOption, ClientWBState, ClientWBStateData}

/**
  * Created by dupriez on 27/06/16.
  *
  * To receive and send messages from and to the server
  */
object WebBuilderClientInterface {
  var lastAppliedClientStateID: Int = 0

  def applyNewClientState(clientWBState: ClientWBState): Unit = {
    println(s"Client is applying a new ClientWBState, with stateID ${clientWBState.stateID}")

    def printlnClientWBStateData(clientWBStateData: ClientWBStateData) = {
      clientWBStateData match {
        case ClientWBStateData(idOfCorrespondingWBStateData, sourceCode, idedWebPage, positionsOfModificationsInSourceCode) =>
          println(" idOfCorrespondingWBStateData: "+idOfCorrespondingWBStateData)
          println(" SourceCode: "+sourceCode)
          println(" idedWebPage: "+idedWebPage)
          println(" positionsOfModificationsInSourceCode: "+positionsOfModificationsInSourceCode)
      }
    }

    clientWBState.stateData match {
      case cwbsd@ClientWBStateData(idOfCorrespondingWBStateData, sourceCode, idedWebPage, positionsOfModificationsInSourceCode) =>
        println("Displayed state:")
        printlnClientWBStateData(cwbsd)
        EditorManipulator.updateEditor(
          clientWBState.stateData.sourceCode,
          triggerOnChangeCallback=false,
          positionsOfModificationsInSourceCode
        )
        ScalaJS_Main.renderWebPage(idedWebPage)
    }
    clientWBState.clientClarificationData_option match {
      case Some(ClientClarificationData(clarificationOptions, idOfToBeClarifiedWebElement)) =>
        println("Clarification received")
        def printlnClarificationOption(index: Int, clientClarificationOption: ClientClarificationOption) = {
          clientClarificationOption match {
            case ClientClarificationOption(clientWBStateData, textOfClarifiedWebElement) =>
//            case ClientClarificationOption(sourceCode, idedWebPage, positionsOfModificationsInSourceCode, textOfClarifiedWebElement, idOfCorrespondingWBStateData) =>
              println("ClarificationOption "+index+":")
              printlnClientWBStateData(clientWBStateData)
              println(" textOfClarifiedWebElement: "+textOfClarifiedWebElement)
          }
        }
        List.range(1, clarificationOptions.length-1).foreach((i:Int)=> {printlnClarificationOption(i, clarificationOptions(i-1))})
        ClarificationBox.setSolutionButtons_(clarificationOptions, idOfToBeClarifiedWebElement)
      case None =>
    }
    lastAppliedClientStateID = clientWBState.stateID
  }

  def sendStringModification(stringModification: StringModification, idOfCorrespondingWBStateData_option: Option[Int] = None): Unit = {
    Server ![NewClientWebBuildingState](
      SendStringModification(stringModification, lastAppliedClientStateID, idOfCorrespondingWBStateData_option),
      {
        case NewClientWebBuildingState(newClientWBState) => applyNewClientState(newClientWBState)
      }
      )
  }

//  There's also the editor itself that send DoUpdateCode events to the server (or something along those lines)
}