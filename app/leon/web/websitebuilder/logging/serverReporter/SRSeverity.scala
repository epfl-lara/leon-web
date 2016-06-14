package leon.web.websitebuilder
package logging.serverReporter

/**
  * Created by dupriez on 2/15/16.
  */
//"SR" -> "Server Reporter"
abstract sealed class SRSeverity {
  val severityString: String
}
case object Info extends SRSeverity {
  override val severityString: String = "[INFO]"
}
case object Error extends SRSeverity {
  override val severityString: String = "[ERROR]"
}
case object Debug extends SRSeverity {
  override val severityString: String = "[DEBUG]"
}

