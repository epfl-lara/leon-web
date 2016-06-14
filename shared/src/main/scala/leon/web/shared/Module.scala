package leon.web
package shared

sealed trait Module { def name: String }
case object Main extends Module { val name= "main" }
case object Verification extends Module { val name= "verification" }
case object Termination extends Module { val name= "termination" }
case object Synthesis extends Module { val name= "synthesis" }
case object Execution extends Module { val name= "execution" }
case object Repair extends Module { val name= "repair" }
case object Invariant extends Module { val name= "invariant" }
case object Disambiguation extends Module { val name= "disambiguation" }
case object Git extends Module { val name= "git" }
case object WebsiteBuilder extends Module { val name = "websitebuilder" }

object Module {
  import boopickle.Default._
  
  implicit val modulePickler = generatePickler[Module]
}
