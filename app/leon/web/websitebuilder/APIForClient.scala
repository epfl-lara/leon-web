package leon.web.websitebuilder

import leon.web.shared.StringModification
import main.scala.leon.web.shared.webBuilding.{SerialisedClientWBState}

/**
  * Created by dupriez on 27/06/16.
  *
  * Defines the functions to call when receiving a message from the client
  */
object APIForClient {
  def requestInitialClientWBState(): SerialisedClientWBState = {
    ???
  }
  def sourceCodeChange(newSourceCode: String): SerialisedClientWBState = {
    ???
  }
  def stringModification(stringModification: StringModification, baseServerWBStateID: Int): SerialisedClientWBState = {
    ???
  }
}
