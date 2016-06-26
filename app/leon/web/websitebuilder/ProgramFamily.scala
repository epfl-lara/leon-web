package leon.web.websitebuilder

import leon.purescala.Definitions.Program

/**
  * Created by dupriez on 26/06/16.
  * Distinguish and provides operations to mutate programs and source codes with the same structure
  */
object ProgramFamily {
  private var familyIDCounter = 0
  def provideFamilyID() = {
    familyIDCounter += 1
    familyIDCounter
  }
}

class ProgramFamily {
  val familyID = ProgramFamily.provideFamilyID()
}
