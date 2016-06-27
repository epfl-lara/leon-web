package leon.web.websitebuilder.clarification

import main.scala.leon.web.shared.webBuilding.WebElementID

/**
  * Created by dupriez on 27/06/16.
  */

/**
  *
  * @param idOfInvolvedWebElement
  * @param resultString
  * @param equationBounds: The bounds on the indexes in the list of the involved webElement's StringLiteral of the
  *                      StringLiterals that must be considered for the equation
  */
case class LocalStringEquation(
                                idOfInvolvedWebElement: WebElementID,
                                resultString: String,
                                equationBounds: (Int,Int)
                              )
