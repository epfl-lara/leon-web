package leon.web
package models

import leon._
import leon.purescala.Definitions._
import leon.synthesis._

class WebSynthesizer(cs: BaseActor,
                     ctx: LeonContext,
                     program: Program,
                     ci: SourceInfo,
                     settings: SynthesisSettings) extends Synthesizer(ctx, program, ci, settings) {


  lazy val search = new SimpleWebSearch(cs, ctx, ci, problem, CostModels.default, Some(200))

  override def getSearch() = search

}
