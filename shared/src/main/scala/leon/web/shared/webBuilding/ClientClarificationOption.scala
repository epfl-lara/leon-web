package main.scala.leon.web.shared.webBuilding

import leon.web.shared.StringPositionInSourceCode
import leon.webDSL.webDescription.WebPageWithIDedWebElements

/**
  * Created by dupriez on 27/06/16.
  */

case class ClientClarificationOption(
                                      clientWBStateData: ClientWBStateData,
                                      textOfClarifiedWebElement: String
                                    )