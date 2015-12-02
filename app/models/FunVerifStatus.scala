package leon.web
package models

import leon.verification._
import leon.purescala.Definitions._
import leon.evaluators._
import leon.purescala.Expressions._
import leon.web.shared.VerifStatus

case class FunVerifStatus(fd: FunDef,
                          results: Map[VC, (Option[VCResult], Option[EvaluationResults.Result[Expr]])],
                          isCondValid: Boolean = false,
                          verifCrashed: Boolean = false) {

  lazy val vcData = results.toSeq.map {
    case (vc, (ores, cexExec)) => (vc, ores.getOrElse(VCResult.unknown), cexExec) 
  }

  lazy val totalTime: Long = vcData.flatMap(_._2.timeMs).foldLeft(0L)(_ + _)

  lazy val overallStatus = {
    val rs = vcData.map(_._2)

    if (rs.exists(_.isInvalid)) {
      VerifStatus.invalid
    } else if (rs.exists(_.status == VCStatus.Timeout)) {
      VerifStatus.timeout
    } else if (vcData.isEmpty || rs.forall(_.isValid)) {
      VerifStatus.valid
    } else if (verifCrashed || rs.exists(_.status == VCStatus.Crashed)) {
      VerifStatus.crashed
    } else {
      VerifStatus.undefined
    }
  }

  lazy val status: String = {
    if (isCondValid && overallStatus == "valid") {
      VerifStatus.cond_valid
    } else {
      overallStatus
    }
  }
}
