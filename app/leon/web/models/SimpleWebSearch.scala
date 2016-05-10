package leon.web
package models

import leon._
import leon.synthesis._
import leon.synthesis.graph._
import play.api.libs.json.Json._
import leon.synthesis.strategies.CostBasedStrategy
import leon.synthesis.strategies.BoundedStrategy

class SimpleWebSearch(cs: BaseActor,
                      ctx: LeonContext,
                      ci: SourceInfo,
                      p: Problem,
                      costModel: CostModel,
                      bound: Option[Int]
                      ) extends Search(ctx, ci, p, {
                          val cbs = new CostBasedStrategy(ctx, costModel)
                          bound match {
                            case Some(bound) => BoundedStrategy(cbs, bound)
                            case None => cbs
                          }
                      }) {

  override def doExpand(n: Node, sctx: SynthesisContext) = {
    if (!n.isExpanded) {
      super.doExpand(n, sctx)
      val (closed, total) = g.getStats()
  
      cs.event(shared.messages.HSynthesisResult(
        result = "progress",
        closed = closed,
        total = total
      ))
    }
  }
}
