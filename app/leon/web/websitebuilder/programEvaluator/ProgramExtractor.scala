package leon.web.websitebuilder
package programEvaluator

import leon.purescala.Definitions.{CaseClassDef, Program}
import leon.purescala.Expressions.Expr
import logging.OptionValWithLog
import logging.serverReporter._


class ProgramExtractor(val program: Program) {
  private def lookupCaseClass(caseClassFullName: String, serverReporter: ServerReporter): OptionValWithLog[CaseClassDef] = {
    val sReporter = serverReporter.startProcess("Source Map looks up the caseClassDef of: \"" + caseClassFullName+"\"")
    program.lookupCaseClass(caseClassFullName) match {
      case Some(classDef) =>
        sReporter.report(Info, "Success")
        OptionValWithLog(Some(classDef), "")
      case None =>
        sReporter.report(Error, "Look up gave no result")
        OptionValWithLog(None, "Failed Source Map lookup for the caseClassDef of: \"" + caseClassFullName+"\"")
    }
  }

  private def lookupCaseClass(caseClassFullName: String): (OptionValWithLog[CaseClassDef], String) = {
    program.lookupCaseClass(caseClassFullName) match {
      case Some(classDef) =>
        (OptionValWithLog(Some(classDef), ""), s"$caseClassFullName: Success,")
      case None =>
        (OptionValWithLog(None, "Failed Source Map lookup for the caseClassDef of: \"" + caseClassFullName+"\""), s"$caseClassFullName: Failure,")
    }
  }

  private var wrappedUpCaseClassDefMapMemoisation: Option[Map[String,OptionValWithLog[CaseClassDef]]] = None

  private val caseClassDefNameAndFullNameMap = Map(
    "Element" -> "leon.webDSL.webDescription.Element",
    "TextElement" -> "leon.webDSL.webDescription.TextElement",
    "WebAttribute" -> "leon.webDSL.webDescription.WebAttribute",
    "WebStyle" -> "leon.webDSL.webDescription.WebStyle",
    "WebPage" -> "leon.webDSL.webDescription.WebPage",
    "leonListCons" -> "leon.collection.Cons",
    "leonListNil" -> "leon.collection.Nil"
  )

  def getWrappedUpCaseClassDefMap(serverReporter: ServerReporter): Map[String,OptionValWithLog[CaseClassDef]] = {
    val sReporter  = serverReporter.startFunction("Looking up Case Class Defs")
    wrappedUpCaseClassDefMapMemoisation match {
      case Some(memoisedValue) =>
          sReporter.report(Info, "Loading them from storage")
          memoisedValue
      case None =>
  //        sReporter.report(Info, "Looking them up in the program: ")
        val (log: String, wrappedUpCaseClassDefMap) =
          caseClassDefNameAndFullNameMap.foldLeft(("Looking them up in the program: ", Map[String, OptionValWithLog[CaseClassDef]]())){
            case ((log:String, underConstructionMap), (caseClassName: String, caseClassFullName: String)) =>
              val lookupResult = lookupCaseClass(caseClassFullName)
              (log + lookupResult._2, underConstructionMap + (caseClassName -> lookupResult._1))
          }
        wrappedUpCaseClassDefMapMemoisation = Some(wrappedUpCaseClassDefMap)
        sReporter.report(Info, log)
        wrappedUpCaseClassDefMap
    }
  }
}
