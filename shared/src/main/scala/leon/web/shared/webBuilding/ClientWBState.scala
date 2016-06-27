package main.scala.leon.web.shared.webBuilding

/**
  * Created by dupriez on 27/06/16.
  */

/**
  * Contains a subset of the information of a WBState, so that it can be serialised and shipped to the client.
  *
  * The String in clarificationData_option is the text of the clarified element according to the coupled ClientWBStateData
  */
case class ClientWBState(stateID: Int, stateData: ClientWBStateData, clientClarificationData_option: Option[ClientClarificationData])
