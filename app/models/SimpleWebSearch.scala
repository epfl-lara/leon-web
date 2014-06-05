package leon.web
package models

import leon.synthesis._
import leon.synthesis.search._
import play.api.libs.json.Json._

class SimpleWebSearch(cs: BaseActor,
                      synth: Synthesizer,
                      problem: Problem) extends SimpleSearch(synth, problem, synth.options.costModel) {

  override def searchStep() {
    super.searchStep()

    val (closed, total) = g.getStatus

    cs.event("synthesis_result", Map(
      "result" -> toJson("progress"),
      "closed" -> toJson(closed),
      "total" -> toJson(total)
    ))
  }

}
