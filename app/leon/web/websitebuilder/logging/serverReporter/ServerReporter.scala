package leon.web.websitebuilder
package logging.serverReporter

import scala.collection.mutable

/**
  * Collect the messages it is given
  */
class ServerReporter(val tabLevel: Int = 0, parent: Option[ServerReporter] = None, processName: String = "", functionName: String = "") {
  private val messageQueue = mutable.Queue[SRMessage]()

  def startProcess(newProcessName: String): ServerReporter = {
    this.report(Info, "Start process: "+newProcessName+"...")
    new ServerReporter(this.tabLevel + 1, Some(this), newProcessName, "")
  }

  def startFunction(newFunctionName: String): ServerReporter = {
    this.report(Info, "Start function: "+newFunctionName+"...")
    new ServerReporter(this.tabLevel + 1, Some(this), this.processName, this.functionName)
  }

  def addTab: ServerReporter = {
    new ServerReporter(this.tabLevel + 1, Some(this), "", "")
  }

  def report(severity: SRSeverity, rawMessage: String): Unit = {

    val srMessage = SRMessage(rawMessage, severity, tabLevel, processName, functionName)
    messageQueue.enqueue(srMessage)
    reportToParent(srMessage)
    println(srMessage.messageToString)
  }

  private def reportToParent(srMessage: SRMessage) = {
    parent.foreach(p => p._reportForSons(srMessage))
  }

  /**
    * To be called by the sons of this reporter to transmit their reports.
    *
    * @param srMessage
    */
  def _reportForSons(srMessage: SRMessage) : Unit = {
    messageQueue.enqueue(srMessage)
    reportToParent(srMessage)
  }

  def flushMessageQueue(whatToDoWithTheOutputString: String=>Unit) = {
    val outputString = messageQueue.foldLeft("")((acc, msg)=> acc + msg.messageToString + sys.props("line.separator"))
    whatToDoWithTheOutputString(outputString)
  }
}
