package leon.web.websitebuilder

import leon.web.websitebuilder.state.WBState
import leon.web.websitebuilder.logging.serverReporter.{Info, ServerReporter}

import scala.collection.mutable

/**
  * Created by dupriez on 27/06/16.
  */
object WBStateMemory {
  private val maxLengthOfWbStateQueue = 5
  private val wbStateQueue = mutable.Queue[WBState]()

  def getWBStateFromIDOrThrowException(askedWBStateID: Int, serverReporter: ServerReporter): WBState = {
    val sReporter = serverReporter.startFunction(s"Retrieving WBState with id $askedWBStateID from Memory")
    wbStateQueue.find((wbState) => wbState.stateID == askedWBStateID) match {
      case Some(wbState) =>
        sReporter.report(Info, "Found")
        wbState
      case None =>
        sReporter.report(Info, "Not found, throwing exception")
        throw WBStateNotFoundException(s"Memory failed to retrieve the WBState of id $askedWBStateID")
    }
  }

  def registerWBState(wbStateToRegister: WBState, serverReporter: ServerReporter): Unit = {
    val sReporter = serverReporter.startFunction(s"Registering new WBState of id ${wbStateToRegister.stateID} in Memory")
    wbStateQueue += wbStateToRegister
    if(wbStateQueue.length > maxLengthOfWbStateQueue){
      val dequeuedWBState = wbStateQueue.dequeue()
      sReporter.report(Info, s"Max length of wbStateQueue reached, removing WBState with id ${dequeuedWBState.stateID}")
    }
  }

}

case class WBStateNotFoundException(msg:String) extends Exception
