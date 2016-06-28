package leon.web.websitebuilder

import leon.frontends.scalac.{ClassgenPhase, ExtractionPhase}
import leon.{DefaultReporter, LeonContext, LeonFatalError, Pipeline}
import leon.purescala.Definitions.Program
import leon.purescala.MethodLifting
import leon.utils.TemporaryInputPhase
import leon.web.websitebuilder.state.SourceCodeAndProgram_StronglyBinded
import leon.web.websitebuilder.logging.serverReporter.{Error, Info, ServerReporter}

/**
  * Created by dupriez on 27/06/16.
  * A replacement for LeonProgramMaker
  */
object Compiler {

  case class CompilingException(msg: String) extends Exception

  def compile(sourceCode: String, serverReporter: ServerReporter) : SourceCodeAndProgram_StronglyBinded = {
    val sReporter = serverReporter.startProcess("Turning the sourceCode into a leon Program")
    val leonReporter = new DefaultReporter(Set())
    val ctx = leon.Main.processOptions(Seq()).copy(reporter = leonReporter)
    ctx.interruptManager.registerSignalHandler()

    val pipeline: Pipeline[List[String], Program] =
      ClassgenPhase andThen
        ExtractionPhase andThen
        MethodLifting
    val pipelineInput = TemporaryInputPhase(ctx, (List(sourceCode), List()))

    def runPipeline(pipeline: Pipeline[List[String], Program], pipelineInput: List[String], leonContext: LeonContext) : Program = {
      try {
        sReporter.report(Info, "Running leon pipeline")
        val (context, program) = pipeline.run(leonContext,pipelineInput)
        sReporter.report(Info, "Leon pipeline run ended")
        program
      } catch {
        case e: Throwable =>
          sReporter.report(Error, "Exception launched during compilation of source code")
          throw CompilingException("Exception launched during compilation of source code: "+sourceCode)
//        case e: LeonFatalError =>
//          sReporter.report(Error, "Leon pipeline run failed, LeonFatalError caught")
//          None
//        case e: Throwable =>
//          sReporter.report(Error, "Leon pipeline run failed, exception other than LeonFatalError caught")
//          None
      }
    }
    val program = runPipeline(pipeline, pipelineInput, ctx)
    sReporter.report(Info, "Generated Program: "+"DISABLED (to re-enable it, look for \"#VERBOSITY\" in LeonProgramMaker.scala)")
    //        #VERBOSITY
    //        sReporter.report(Info, "Generated Program: "+program)
    new SourceCodeAndProgram_StronglyBinded(sourceCode, program)
//    runPipeline(pipeline, pipelineInput, ctx) match {
//      case Some(program) =>
//        sReporter.report(Info, "Generated Program: "+"DISABLED (to re-enable it, look for \"#VERBOSITY\" in LeonProgramMaker.scala)")
//        //        #VERBOSITY
//        //        sReporter.report(Info, "Generated Program: "+program)
//        Some(new SourceCodeAndProgram_StronglyBinded(sourceCode, program))
//      case _ =>
//        None
//    }
  }
}
