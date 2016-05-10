package leon.web
package models

import play.api.libs.json._
import play.api.libs.json.Json._
import leon._
import leon.evaluators._
import leon.verification._
import leon.utils._
import leon.purescala.Common._
import leon.purescala.Expressions._
import leon.transformations.InstUtil
import leon.purescala.Definitions._
import leon.purescala.PrettyPrinter
import leon.purescala.SelfPrettyPrinter

trait JsonWrites {
  implicit val ctx: LeonContext;
  
  protected def program: Option[Program]
  
  /** Caches expr to string transformation to revert them back. */
  protected var activateCache: Boolean = false
  def updateExprCache(s: String, e: Expr) = if(activateCache) exprCache += s -> e
  def getExprFromCache(s: String): Option[Expr] = if(activateCache) exprCache.get(s) else None
  def clearExprCache() = exprCache = Map()
  protected var exprCache = Map[String, Expr]()
  
  /*implicit val doWrites = new Writes[DualOutput] {
    def writes(d: DualOutput) = {
      Json.obj(
          "rawoutput" -> d.rawoutput,
          "prettyoutput" -> d.prettyoutput
      )
    }
  } 

  implicit val erWrites = new Writes[EvaluationResults.Result[Expr]] {
    def writes(er: EvaluationResults.Result[Expr]) = er match {
      case EvaluationResults.Successful(ex) =>
        val rawoutput = ex.asString
        val exAsString = program.map(p => SelfPrettyPrinter.print(ex, ex.asString)(ctx, p)).getOrElse(rawoutput)
        updateExprCache(rawoutput, ex)
        Json.obj(
          "result" -> "success",
          "output" -> DualOutput(rawoutput, exAsString)
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
        case (id, expr) =>
          val rawoutput = expr.asString
          val exprAsString = program.map(p => SelfPrettyPrinter.print(expr, rawoutput)(ctx, p)).getOrElse(rawoutput)
          updateExprCache(rawoutput, expr)
          id.asString -> (DualOutput(rawoutput, exprAsString): JsValueWrapper)
      } :_*
    )
  }
  
  implicit val idMapStringWrites = new Writes[Map[Identifier, String]] {
    def writes(ex: Map[Identifier, String]) = Json.obj(
      ex.toSeq.sortBy(_._1).map {
        case (id, expr) => id.asString -> (expr: JsValueWrapper)
      } :_*
    )
  }


  implicit val vrWrites = new Writes[(VC, VCResult, Option[EvaluationResults.Result[Expr]])] {
    def writes(vr: (VC, VCResult, Option[EvaluationResults.Result[Expr]])) = {
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

  implicit val fiWrites = new Writes[FunInvariantStatus] {
    def writes(fi: FunInvariantStatus) = Json.obj(
      "status" -> fi.status,
      "fun" -> fi.fd.map(InstUtil.userFunctionName(_)).getOrElse("").asInstanceOf[String],
      "oldInvariant" -> fi.template.getOrElse("").asInstanceOf[String],
      "newInvariant"   -> fi.invString.getOrElse("").asInstanceOf[String],
      "newCode"    -> fi.newCode.getOrElse("").asInstanceOf[String],
      "time"    -> fi.time.getOrElse(0.0).asInstanceOf[Double]
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
  }*/
}
