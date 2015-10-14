package leon.web.shared

object Constants {

}

object Action {
  val featureSet = "featureSet"
  val accessPermaLink = "accessPermaLink"
  val doUpdateCode = "doUpdateCode"
  val storePermaLink = "storePermaLink"
  val getRulesToApply = "getRulesToApply"
  val doCancel = "doCancel"
  val doExplore = "doExplore"
  val doSearch = "doSearch"
  val doApplyRule = "doApplyRule"
  val doRepair = "doRepair"
}

object VerifStatus {
  val invalid = "invalid"
  val valid = "valid"
  val crashed = "crashed"
  val undefined = "undefined"
  val timeout = "timeout"
  val cond_valid = "cond-valid"
  val unknown = "unknown"
}

object TerminationStatus {
  val terminates = "terminates"
  val loopsfor = "loopsfor"
  val callsnonterminating = "callsnonterminating"
  val noguarantee = "noguarantee"
  /** "work in progress", will display spinning arrows */
  val wip = "wip"
}

object Module {
    val verification = "verification"
    val termination = "termination"
    val synthesis = "synthesis"
    val execution = "execution"
    val repair = "repair"
    val invariant = "invariant"
}