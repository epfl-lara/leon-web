package main.scala.leon.web.client.websitebuilder

import leon.web.client.Main.Server
import leon.web.client.websitebuilder.ScalaJS_Main
import leon.web.shared.StringModification
import leon.web.shared.messages.{NewClientWebBuildingState, SendStringModification}
import main.scala.leon.web.client.websitebuilder.state._
import main.scala.leon.web.shared.webBuilding.SerialisedClientWBState

/**
  * Created by dupriez on 27/06/16.
  *
  * To receive and send messages from and to the server
  */
object WebBuilderClientInterface {
  var lastAppliedClientStateID: Int = 0

  private def printlnClientWBStateData(clientWBStateData: ClientWBStateData_Client) = {
    clientWBStateData match {
      case ClientWBStateData_Client(idOfCorrespondingWBStateData, sourceCode, idedWebPage, positionsOfModificationsInSourceCode) =>
        println(" idOfCorrespondingWBStateData: "+idOfCorrespondingWBStateData)
        println(" SourceCode: "+sourceCode)
        println(" idedWebPage: "+idedWebPage)
        println(" positionsOfModificationsInSourceCode: "+positionsOfModificationsInSourceCode)
    }
  }

  private def printlnClarificationOption(index: Int, clientClarificationOption: ClientClarificationOption_Client) = {
    clientClarificationOption match {
      case ClientClarificationOption_Client(clientWBStateData, textOfClarifiedWebElement) =>
        //            case ClientClarificationOption(sourceCode, idedWebPage, positionsOfModificationsInSourceCode, textOfClarifiedWebElement, idOfCorrespondingWBStateData) =>
        println("ClarificationOption "+index+":")
        printlnClientWBStateData(clientWBStateData)
        println(" textOfClarifiedWebElement: "+textOfClarifiedWebElement)
    }
  }

  def receiveNewSerialisedClientState(newSerialisedClientWBState: SerialisedClientWBState): Unit = {
    println(s"Client has received a SerialisedClientWBState, with stateID: ${newSerialisedClientWBState.stateID}")
    val newClientWBState = SerialisedClientWBStateDeserialiser.deserialise(newSerialisedClientWBState)
    applyClientWBStateData(newClientWBState.stateData)
    newClientWBState.clientClarificationData_option match {
      case Some(clarificationData) => displayClarificationData(clarificationData)
      case None =>
    }
    lastAppliedClientStateID = newSerialisedClientWBState.stateID
  }

  def applyClientWBStateData(clientWBStateData: ClientWBStateData_Client): Unit = {
    clientWBStateData match {
      case cwbsd@ClientWBStateData_Client(idOfCorrespondingWBStateData, sourceCode, idedWebPage, positionsOfModificationsInSourceCode) =>
        println("Displayed state:")
        printlnClientWBStateData(cwbsd)
        EditorManipulator.updateEditor(
          sourceCode,
          triggerOnChangeCallback=false,
          positionsOfModificationsInSourceCode
        )
        ScalaJS_Main.renderWebPage(idedWebPage)
    }
  }

  private def displayClarificationData(clientClarificationData: ClientClarificationData_Client): Unit = {
    clientClarificationData match {
      case ClientClarificationData_Client(clarificationOptions, idOfToBeClarifiedWebElement) =>
        println("ClarificationData received")
        List.range(1, clarificationOptions.length-1).foreach((i:Int)=> {printlnClarificationOption(i, clarificationOptions(i-1))})
        ClarificationBox.setSolutionButtons_(clarificationOptions, idOfToBeClarifiedWebElement)
    }
  }

  def sendStringModification(stringModification: StringModification, idOfCorrespondingWBStateData_option: Option[Int] = None): Unit = {
    Server ![NewClientWebBuildingState](
      SendStringModification(stringModification, lastAppliedClientStateID, idOfCorrespondingWBStateData_option),
      {
        case NewClientWebBuildingState(newSerialisedClientWBState) => receiveNewSerialisedClientState(newSerialisedClientWBState)
      }
      )
  }

//  There's also the editor itself that send DoUpdateCode events to the server (or something along those lines)
}