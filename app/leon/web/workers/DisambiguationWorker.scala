package leon
package web
package workers

import akka.actor._
import play.api.libs.json.Json._
import scala.concurrent.duration._
import models._
import leon.LeonContext
import leon.utils._
import leon.purescala.PrinterOptions
import leon.purescala.PrinterContext
import leon.purescala.ScalaPrinter
import leon.purescala.Definitions.FunDef
import leon.synthesis._
import leon.purescala.Common._
import leon.purescala.ExprOps._
import leon.purescala.Constructors._
import leon.purescala.Expressions._
import leon.purescala.Types._
import leon.web.shared.Action
import leon.purescala.DefOps
import leon.purescala.ExprOps
import leon.synthesis.disambiguation._
import leon.purescala.Definitions.Program
import play.api.libs.json._
import play.api.libs.json.Json._
import leon.grammars.{ValueGrammar, ExpressionGrammar}

class DisambiguationWorker(s: ActorRef, im: InterruptManager) extends WorkerActor(s, im) {
  import ConsoleProtocol._
  
  def convertExampleToFullCode(cstate: CompilationState, synth: Synthesizer, in: Expr, out: Expr): String = {
    val ci = synth.ci
    val SourceInfo(fd, src, pb) = ci //pc, , spec, tb

    leon.web.utils.FileInterfaceWeb.allCodeWhereFunDefModified(fd)(nfd => {
      val ea = new ExamplesAdder(synth.context, cstate.program)
      ea.setRemoveFunctionParameters(true)
      ea.addToFunDef(nfd, Seq((in, out)))
    })(cstate, synth.context)
  }
  
  import shared.messages.{DoCancel => _, _}
  def convertQuestion(cstate: CompilationState, synth: Synthesizer, fd: FunDef, question: Question[Expr]): HDisambiguationResult = {
    val (in, mapping) = ExamplesAdder.replaceGenericValuesByVariable(tupleWrap(question.inputs))
    
    @inline def instantiate(x: Expr) = ExprOps.replace(mapping, x)
    
    HDisambiguationResult(input = in.asString,
        fname = fd.id.name,
        confirm_solution = HDisambiguationDisplay(
                instantiate(question.current_output).asString(synth.program)(synth.context), 
                convertExampleToFullCode(cstate, synth, in, instantiate(question.current_output))),
        custom_alternative = HDisambiguationDisplay(
                shared.Constants.disambiguationPlaceHolder, 
                convertExampleToFullCode(cstate, synth, in, StringLiteral(shared.Constants.disambiguationPlaceHolder))),
       alternatives =
            question.other_outputs.map(output => HDisambiguationDisplay(
                instantiate(output).asString(synth.program)(synth.context), 
                convertExampleToFullCode(cstate, synth, in, instantiate(output)))))
  }
  
  def isGround(s: String): Boolean = {
    s == s.replaceAll(leon.synthesis.rules.StringRender.EDIT_ME, "")
  }
  
  /** Returns the expression if it is different from the previous ones, else None */
  def filterRedundantExprs(prev: Seq[Expr], current: Expr): Option[Expr] = {
    val currentStr = current.toString
    val currentStrSimp = currentStr.replaceAll(leon.synthesis.rules.StringRender.EDIT_ME, "")
    if(prev.exists { prev => val prevStr = prev.toString
        val prevStrSimp = prevStr.replaceAll(leon.synthesis.rules.StringRender.EDIT_ME, "")
        (!isGround(prevStr) && prevStrSimp == currentStrSimp) || ExprOps.canBeHomomorphic(prev, current).nonEmpty
      })
      None
    else
      Some(current)
  }
  import leon.grammars._
  
  /** Specific enumeration of strings, which can be used with the QuestionBuilder#setValueEnumerator method */
  object NonEmptyValueGrammarfirst extends SimpleExpressionGrammar {
    def computeProductions(t: TypeTree)(implicit ctx: LeonContext): Seq[ProductionRule[TypeTree, Expr]] = t match {
       case StringType =>
          List(
            terminal(StringLiteral("foo"), Tags.Constant),
            terminal(StringLiteral("\"'\n\t"), Tags.Constant)
            //terminal(StringLiteral("Lara 2007"))
          )
       case tp: TypeParameter =>
          List(
            terminal(GenericValue(tp, 1))
          )
       case _ => ValueGrammar.computeProductions(t)
    }
  }
  
  def receive = {
    case OnUpdateCode(cstate) =>
      // Do nothing, wait for the synthesis to complete.

    case DoCancel =>
      sender ! Cancelled(this)

    case NewSolutions(cstate, synth, ssol) =>
      logInfo("Receiving new solutions ! disambiguating ...")
      import shared.messages._
      event(DisambiguationStarted)
      val ci = synth.ci
      val SourceInfo(fd, src, pb) = ci
      
      val qb = new QuestionBuilder(fd.paramIds.filter(x => !x.getType.isInstanceOf[FunctionType]), ssol, filterRedundantExprs)(synth.context, cstate.program)
      qb.setSortAlternativesBy(QuestionBuilder.AlternativeSortingType.BalancedParenthesisIsBetter())
      qb.setKeepEmptyAlternativeQuestions { case s => !isGround(s.asString) }
      qb.setValueEnumerator(NonEmptyValueGrammarfirst)
      val questions = qb.result()
      
      if(questions.nonEmpty) {
        logInfo("Sending back results")
        event(convertQuestion(cstate, synth, fd, questions.head))
      } else {
        event(DisambiguationNoresult)
      }
      logInfo("Finished disambiguating")
      
    case OnClientEvent(cstate, event) =>

    case _ =>
  }
}
