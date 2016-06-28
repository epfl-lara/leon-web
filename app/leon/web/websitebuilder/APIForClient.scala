package leon.web.websitebuilder

import leon.web.shared.StringModification
import leon.web.websitebuilder.logging.serverReporter.{Info, ServerReporter}
import leon.web.websitebuilder.programEvaluator.ProgramRunner
import leon.web.websitebuilder.state.{ClientWBStateSerialiser, SourceCodeAndProgram_StronglyBinded, WBState, WBStateData}
import leon.web.websitebuilder.webPageExprConversion.{SourceMap, WebPageExprToIdedWebPageAndSourceMap}
import main.scala.leon.web.shared.webBuilding.SerialisedClientWBState

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by dupriez on 27/06/16.
  *
  * Defines the functions to call when receiving a message from the client
  */
object APIForClient {
  def requestInitialClientWBState(): SerialisedClientWBState = {
    ???
//    TODO: Implement this. And use it.
  }
  def sourceCodeChange(newSourceCode: String): SerialisedClientWBState = {
    val serverReporter = new ServerReporter()
    serverReporter.report(Info, "Receiving a new source code: "+newSourceCode)
    val sCodeAndProgramBinding: SourceCodeAndProgram_StronglyBinded = Compiler.compile(newSourceCode, serverReporter)
    val (concreteExprOfWebPage, abstractExprOfWebPage) = {
      val fullNameOfMainFunction = "Main.main"
      (
        ProgramRunner.runProgram_concrete(sCodeAndProgramBinding.program, fullNameOfMainFunction, serverReporter),
        ProgramRunner.runProgram_abstract(sCodeAndProgramBinding.program, fullNameOfMainFunction, serverReporter)
      )
    }
    val (idedWebPage, abstractExprOfWebPageToSourceMapFunction) =
      WebPageExprToIdedWebPageAndSourceMap.unexprAndIDWebPageAndStartBuildingSourceMap(
        concreteExprOfWebPage,
        sCodeAndProgramBinding.program,
        sCodeAndProgramBinding.sourceCode,
        serverReporter
      )
    val sourceMap_future = Future { abstractExprOfWebPageToSourceMapFunction(abstractExprOfWebPage) }

    val newWBStateData: WBStateData = WBStateData(
      sCodeAndProgramBinding,
      idedWebPage,
      sourceMap_future,
      List()
    )
    val newWBState = WBState(
      newWBStateData,
      None
    )
    WBStateMemory.registerWBState(newWBState, serverReporter)
    ClientWBStateSerialiser.serialise(newWBState.extractToClientWBState)
  }
  def stringModification(stringModification: StringModification, baseServerWBStateID: Int): SerialisedClientWBState = {
    val serverReporter = new ServerReporter()
    val baseWBState = WBStateMemory.getWBStateFromIDOrThrowException(baseServerWBStateID, serverReporter)
    val baseWBStateData = baseWBState.stateData
    val baseClarificationState_option = baseWBState.clarificationState_option

    val newWBStateData: WBStateData = ???
    val newWBState = WBState(
      newWBStateData,
      ???
    )
    WBStateMemory.registerWBState(newWBState, serverReporter)
    ClientWBStateSerialiser.serialise(newWBState.extractToClientWBState)
  }
}
