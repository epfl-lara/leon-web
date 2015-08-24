package leon.web
package models

import leon.verification._
import leon.purescala.Definitions._
import leon.evaluators._

case class FunVerifStatus(fd: FunDef,
                          results: Map[VC, (Option[VCResult], Option[EvaluationResults.Result])],
                          isCondValid: Boolean = false,
                          verifCrashed: Boolean = false) {

  lazy val vcData = results.toSeq.map {
    case (vc, (ores, cexExec)) => (vc, ores.getOrElse(VCResult.unknown), cexExec) 
  }

  lazy val totalTime: Long = vcData.flatMap(_._2.timeMs).foldLeft(0L)(_ + _)

  lazy val overallStatus = {
    val rs = vcData.map(_._2)

    if (rs.exists(_.isInvalid)) {
      "invalid"
    } else if (rs.exists(_.status == VCStatus.Timeout)) {
      "timeout"
    } else if (vcData.isEmpty || rs.forall(_.isValid)) {
      "valid"
    } else if (verifCrashed) {
      "crashed"
    } else {
      "undefined"
    }
  }

  lazy val status: String = {
    if (isCondValid && overallStatus == "valid") {
      "cond-valid"
    } else {
      overallStatus
    }
  }
}
