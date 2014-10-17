package leon.web
package models

import leon._
import leon.synthesis._
import leon.synthesis.graph._
import play.api.libs.json.Json._

class SimpleWebSearch(cs: BaseActor,
                      ctx: LeonContext,
                      problem: Problem) extends SimpleSearch(ctx, problem, CostModel.default, Some(200)) {

  override def doStep(n: g.Node, sctx: SynthesisContext) = {
    super.doStep(n, sctx);

    val (closed, total) = g.getStats()

    cs.event("synthesis_result", Map(
      "result" -> toJson("progress"),
      "closed" -> toJson(closed),
      "total" -> toJson(total)
    ))
  }
}
