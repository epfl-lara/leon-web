package leon.web.websitebuilder.programEvaluator

import leon.{DefaultReporter, LeonContext}
import leon.evaluators._
import leon.purescala.Definitions.{FunDef, Program}
import leon.purescala.Expressions.{Expr, FunctionInvocation}
import leon.web.websitebuilder.logging.serverReporter.{Error, Info, ServerReporter}

/**
  * Created by dupriez on 28/06/16.
  */
object ProgramRunner {

  private def defaultLeonContext() = {
    val ctx = leon.Main.processOptions(Seq()).copy(reporter = new DefaultReporter(Set()))
    ctx.interruptManager.registerSignalHandler()
    ctx
  }

  def runProgram_concrete(program: Program, fullNameOfFunctionToRun: String, serverReporter: ServerReporter): Expr = {
    val sReporter = serverReporter.startFunction("Concretely running program on function \""+fullNameOfFunctionToRun+"\"")
    val ctx = defaultLeonContext()
    val defaultEvaluator = new DefaultEvaluator(ctx, program)
    val funDefToEvaluate = extractFunDef(program, fullNameOfFunctionToRun, sReporter)
    evaluate(defaultEvaluator, funDefToEvaluate, Seq(), sReporter)
  }

  def runProgram_abstract(program: Program, fullNameOfFunctionToRun: String, serverReporter: ServerReporter): Expr = {
    val sReporter = serverReporter.startFunction("Abstractly running program on function \""+fullNameOfFunctionToRun+"\"")
    val ctx = defaultLeonContext()
    val abstractOnlyEvaluator = new AbstractOnlyEvaluator(ctx, program)
    val funDefToEvaluate = extractFunDef(program, fullNameOfFunctionToRun, sReporter)
    evaluate(abstractOnlyEvaluator, funDefToEvaluate, Seq(), sReporter)
  }

  private def extractFunDef(program:Program, fullNameOfFunDefToExtract: String, serverReporter: ServerReporter): FunDef = {
    case class FunDefExtractionException(msg: String) extends Exception
    val sReporter = serverReporter.startFunction("Extracting FunDef from name \""+fullNameOfFunDefToExtract+"\" from program")
    program.lookupFunDef(fullNameOfFunDefToExtract) match {
      case Some(funDef) =>
        sReporter.report(Info, "Success")
        funDef
      case None => {
        val errorMessage = "Failed to extract FunDef from name \""+fullNameOfFunDefToExtract+"\" from program"
        sReporter.report(Error, errorMessage)
        throw FunDefExtractionException(errorMessage)
      }
    }
  }

  private def evaluate(evaluator: ContextualEvaluator, funDefToEvaluate: FunDef, arguments: Seq[Expr]=Seq(), serverReporter: ServerReporter): Expr = {
    case class EvaluationException(msg: String) extends Exception

    val sReporter = serverReporter.startFunction("Evaluating a program")
    evaluator.eval(FunctionInvocation(funDefToEvaluate.typed, arguments)) match {
      case EvaluationResults.Successful(res) => {
        //        Note: in resultEvaluationTreeExpr, the function calls are replaced by their return value
        sReporter.report(Info, "Evaluation successful")
        res match {
          case expr: Expr => expr
          case _ =>
            val errorMessage = "Evaluator returned something that is not an Expr"
            sReporter.report(Error, errorMessage)
            throw EvaluationException(errorMessage)
        }
      }
      case EvaluationResults.EvaluatorError(msg) => {
        val errorMessage = "Evaluation failed: evaluator returned an EvaluationError with message: "+msg
        sReporter.report(Error, errorMessage)
        throw EvaluationException(errorMessage)
      }
      case EvaluationResults.RuntimeError(msg) => {
        val errorMessage = "Evaluation failed: evaluator returned a RuntimeError with message: "+msg
        sReporter.report(Info, errorMessage)
        throw EvaluationException(errorMessage)
      }
    }
  }
}
