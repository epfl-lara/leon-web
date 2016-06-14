package leon.web
package shared

import leon.webDSL.webDescription._

/**
  * Created by dupriez on 3/10/16.
  */

// If modifiedWebAttribute = None, it means that the webElement is a TextElement, and that we want to modify its text>
// If not, then modifiedWebAttribute is the name of the Attribute we want to modify
case class StringModification(webElementID: Int, modifiedWebAttribute: Option[String], newValue: String)