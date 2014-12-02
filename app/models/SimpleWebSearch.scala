package leon.web
package models

import leon._
import leon.synthesis._
import leon.synthesis.graph._
import play.api.libs.json.Json._

class SimpleWebSearch(cs: BaseActor,
                      ctx: LeonContext,
                      p: Problem,
                      costModel: CostModel,
                      bound: Option[Int]
                      ) extends SimpleSearch(ctx, p, costModel, bound) {

  override def doStep(n: Node, sctx: SynthesisContext) = {
    super.doStep(n, sctx);

    val (closed, total) = g.getStats()

    cs.event("synthesis_result", Map(
      "result" -> toJson("progress"),
      "closed" -> toJson(closed),
      "total" -> toJson(total)
    ))
  }
}
