package leon.web.websitebuilder
package logging.serverReporter

/**
  * Created by dupriez on 2/15/16.
  */
//"SR" -> "Server Reporter"
case class SRMessage(rawMessage: String, severity: SRSeverity, tabLevel: Int, processName: String, functionName: String) {
  private def tabAdder(rawMsg: String, tabLvl: Int) = {
    var msg = rawMsg
    for(i <- 0 to tabLvl){
      msg = "  " + msg
    }
    msg
  }
  def messageToString = severity.severityString + " " + tabAdder(rawMessage, tabLevel)
}
