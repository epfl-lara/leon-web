package leon.web
package models

import leon.purescala.Expressions.Expr
import leon.purescala.Definitions.FunDef
import leon.web.shared.VerifStatus
import leon.web.shared.InvariantStatus

/**
 * @author Mikael
 */
case class FunInvariantStatus(fd: Option[FunDef],
                              template: Option[String],
                              invString: Option[String],
                              newCode: Option[String],
                              time: Option[Double],
                              invariantFound: Boolean = false,
                              invariantCrashed: Boolean = false,
                              invariantTimeout: Boolean = false) {

  lazy val status: String = {
   if (invariantTimeout) {
      InvariantStatus.timeout
    } else if (invariantFound) {
      InvariantStatus.found
    } else if (invariantCrashed) {
      InvariantStatus.crashed
    } else {
      InvariantStatus.undefined
    }
  }
}