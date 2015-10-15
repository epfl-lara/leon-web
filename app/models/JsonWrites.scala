package leon.web
package models

import play.api.libs.json._
import play.api.libs.json.Json._

import leon._
import leon.evaluators._
import leon.verification._
import leon.utils._
import leon.purescala._
import leon.purescala.Common._
import leon.purescala.Definitions._
import leon.purescala.Expressions._

trait JsonWrites {
  implicit val ctx: LeonContext;

  implicit val erWrites = new Writes[EvaluationResults.Result] {
    def writes(er: EvaluationResults.Result) = er match {
      case EvaluationResults.Successful(ex) =>
        Json.obj(
          "result" -> "success",
          "output" -> ex.asString
        )

      case EvaluationResults.RuntimeError(msg) =>
        Json.obj(
          "result" -> "error",
          "error"  -> msg
        )

      case EvaluationResults.EvaluatorError(msg) =>
        Json.obj(
          "result" -> "error",
          "error"  -> msg
        )
    }
  }

  implicit val idMapWrites = new Writes[Map[Identifier, Expr]] {
    def writes(ex: Map[Identifier, Expr]) = Json.obj(
      ex.toSeq.sortBy(_._1).map {
        case (id, expr) => id.asString -> (expr.asString: JsValueWrapper)
      } :_*
    )
  }

  implicit val vrWrites = new Writes[(VC, VCResult, Option[EvaluationResults.Result])] {
    def writes(vr: (VC, VCResult, Option[EvaluationResults.Result])) = {
      val (vc, res, cexExec) = vr

      val timeSec = res.timeMs.map(t => f"${t/1000d}%-3.3f").getOrElse("")

      val base = Json.obj(
        "kind"   -> vc.kind.toString,
        "fun"    -> vc.fd.id.name,
        "status" -> res.status.name,
        "time"   -> timeSec
      )

      res.status match {
        case VCStatus.Invalid(cex) =>
          cexExec match {
            case Some(er) =>
              base ++ Json.obj(
                "counterExample" -> cex.toMap[Identifier, Expr],
                "execution"      -> er
              )
            case _ =>
              base ++ Json.obj(
                "counterExample" -> cex.toMap[Identifier, Expr]
              )
          }
        case _ =>
          base
      }
    }
  }

  implicit val fvWrites = new Writes[FunVerifStatus] {
    def writes(fv: FunVerifStatus) = Json.obj(
      "status" -> fv.status,
      "time"   -> fv.totalTime,
      "vcs"    -> fv.vcData.toSeq
    )
  }

  implicit val rpWrites = new Writes[RangePosition] {
    def writes(rp: RangePosition) = Json.obj(
      "fromRow"     -> (rp.lineFrom-1),
      "fromColumn"  -> (rp.colFrom-1),
      "toRow"       -> (rp.lineTo-1),
      "toColumn"    -> (rp.colTo-1)
    )
  }

  implicit val rpResWrites = new Writes[(RangePosition, String)] {
    def writes(rpRes: (RangePosition, String)) =
      rpWrites.writes(rpRes._1) + ("result" -> toJson(rpRes._2))
  }
}
