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

  def element_webElementCaseClassDef(serverReporter: ServerReporter): OptionValWithLog[CaseClassDef] = {
    lookupCaseClass("leon.webDSL.webDescription.Element", serverReporter)
  }
  def textElement_webElementCaseClassDef(serverReporter: ServerReporter): OptionValWithLog[CaseClassDef] = {
    lookupCaseClass("leon.webDSL.webDescription.TextElement", serverReporter)
  }
  def webAttribute_webElementCaseClassDef(serverReporter: ServerReporter): OptionValWithLog[CaseClassDef] = {
    lookupCaseClass("leon.webDSL.webDescription.WebAttribute", serverReporter)
  }
  def webStyle_webElementCaseClassDef(serverReporter: ServerReporter): OptionValWithLog[CaseClassDef] = {
    lookupCaseClass("leon.webDSL.webDescription.WebStyle", serverReporter)
  }
  def webPage_webElementCaseClassDef(serverReporter: ServerReporter): OptionValWithLog[CaseClassDef] = {
    lookupCaseClass("leon.webDSL.webDescription.WebPage", serverReporter)
  }
  def leonCons_caseClassDef(serverReporter: ServerReporter): OptionValWithLog[CaseClassDef] = {
    lookupCaseClass("leon.collection.Cons", serverReporter)
  }
  def leonNil_caseClassDef(serverReporter: ServerReporter): OptionValWithLog[CaseClassDef] = {
    lookupCaseClass("leon.collection.Nil", serverReporter)
  }
  def webPage_StyleSheetCaseClassDef(serverReporter: ServerReporter): OptionValWithLog[CaseClassDef] = {
    lookupCaseClass("leon.webDSL.webDescription.StyleSheet", serverReporter)
  }
  def webPage_StyleRuleCaseClassDef(serverReporter: ServerReporter): OptionValWithLog[CaseClassDef] = {
    lookupCaseClass("leon.webDSL.webDescription.StyleRule", serverReporter)
  }
}
