package leon.web
package websitebuilder
package programEvaluator

import leon.frontends.scalac.{ExtractionPhase, ClassgenPhase}
import leon.utils.TemporaryInputPhase
import leon.{LeonFatalError, LeonContext, Pipeline, DefaultReporter}
import leon.purescala.Definitions.{CaseClassDef, Program}
import logging.serverReporter.{ServerReporter, Info, Error}
import shared.SourceCodeSubmissionResult
import leon.purescala.MethodLifting
import leon.utils.PreprocessingPhase

/**
  * Created by dupriez on 3/22/16.
  */
object LeonProgramMaker {
  def makeProgram(sourceCode: String, serverReporter: ServerReporter) : Option[Program] = {
    val sReporter = serverReporter.startProcess("Turning the sourceCode into a leon Program")
    val leonReporter = new DefaultReporter(Set())
    val ctx = leon.Main.processOptions(Seq()).copy(reporter = leonReporter)
    ctx.interruptManager.registerSignalHandler()
    val pipeline: Pipeline[List[String], Program] =
      ClassgenPhase andThen
        ExtractionPhase andThen
        MethodLifting
        //new PreprocessingPhase()
    //        PrintTreePhase("Output of leon")

    def addImport(sourceCode: String) = {
//      sReporter.report(Info, "Adding imports to source code: " + "import leon.collection._, "+"import leon.webDSL.webDescription._")
//      "import leon.collection._" + sys.props("line.separator") +
//      "import leon.webDSL.webDescription._" + sys.props("line.separator") +
      sourceCode
    }

    val pipelineInput = TemporaryInputPhase(ctx, (List(addImport(sourceCode)), List()))

    case class PipelineRunResult(msg: String, programOption: Option[Program])

    def runPipeline(pipeline: Pipeline[List[String], Program], pipelineInput: List[String], leonContext: LeonContext) : Option[Program] = {
      try {
        sReporter.report(Info, "Running leon pipeline")
        val (context, program) = pipeline.run(leonContext,pipelineInput)
        sReporter.report(Info, "Leon pipeline run ended")
        Some(program)
      } catch {
        case e: LeonFatalError =>
          //          ctx.reporter.errorMessage match {
          //            case Some(msg) => return FailureCompile(if (msg.position != NoPosition) { msg.position+": " } else { "" },
          //              msg.severity.toString,
          //              msg.msg.toString
          //            )
          //            case None =>
          //          }
          //          return SuccessCompile("<h2>Error here:</h2>" + e.getClass + "<br>" + e.getMessage + "<br>" + e.getStackTrace.map(_.toString).mkString("<br>"))
          sReporter.report(Error, "Leon pipeline run failed, LeonFatalError caught")
          None
        case e: Throwable =>
          //          return SuccessCompile("<h2>Error here:</h2>" + e.getClass + "<br>" + e.getMessage + "<br>" + e.getStackTrace.map(_.toString).mkString("<br>"))
          sReporter.report(Error, "Leon pipeline run failed, exception other than LeonFatalError caught")
          None
      }
    }
    runPipeline(pipeline, pipelineInput, ctx) match {
      case Some(program) =>
        sReporter.report(Info, "Generated Program: "+"DISABLED (to re-enable it, look for \"#VERBOSITY\" in LeonProgramMaker.scala)")
//        #VERBOSITY
//        sReporter.report(Info, "Generated Program: "+program)
        Some(program)
      case _ =>
        None
    }
  }
}
