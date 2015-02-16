package leon.web
package models

import leon._
import leon.purescala.Definitions._
import leon.synthesis._
import leon.synthesis.graph._
import play.api.libs.json.Json._

class WebSynthesizer(cs: BaseActor,
                     ctx: LeonContext,
                     program: Program,
                     ci: ChooseInfo,
                     settings: SynthesisSettings) extends Synthesizer(ctx, program, ci, settings) {


  lazy val search = new SimpleWebSearch(cs, ctx, ci, problem, CostModels.default, Some(200))

  override def getSearch() = search

}
