package leon.web.websitebuilder.webPageExprConversion

import leon.purescala.Definitions.{CaseClassDef, Program}
import leon.purescala.Expressions.{CaseClass, Expr}
import leon.purescala.Types.CaseClassType
import leon.web.websitebuilder.logging.serverReporter.{Error, ServerReporter}

/**
  * Created by dupriez on 28/06/16.
  */
class ConversionBuilder(program: Program, serverReporter: ServerReporter) {

  case class CaseClassDefAccessException(msg: String) extends Exception

  private def getOrThrowException(caseClassDefName: String, caseClassDefMap: Map[String, CaseClassDef], serverReporter: ServerReporter): CaseClassDef = {
    caseClassDefMap.get(caseClassDefName) match {
      case Some(caseClassDef) => caseClassDef
      case None =>
        val errorMessage = "Failed to get CaseClassDef for \""+caseClassDefName+"\" from the map"
        serverReporter.report(Error, errorMessage)
        throw CaseClassDefAccessException(errorMessage)
    }
  }

  val wrappedUpCaseClassDefMap = CaseClassDefExtractor.extractCaseClassDefs(program, serverReporter)
  val webPageCaseClassDef = getOrThrowException("WebPage", wrappedUpCaseClassDefMap, serverReporter)
  val textElementCaseClassDef = getOrThrowException("TextElement", wrappedUpCaseClassDefMap, serverReporter)
  val elementCaseClassDef = getOrThrowException("Element", wrappedUpCaseClassDefMap, serverReporter)
  val webStyleCaseClassDef = getOrThrowException("WebStyle", wrappedUpCaseClassDefMap, serverReporter)
  val consCaseClassDef = getOrThrowException("leonListCons", wrappedUpCaseClassDefMap, serverReporter)
  val nilCaseClassDef = getOrThrowException("leonListNil", wrappedUpCaseClassDefMap, serverReporter)

  def exprOfLeonListOfExprToLeonListOfExpr(leonListExpr: Expr) : leon.collection.List[Expr] = {
    val actualLeonListExpr = TupleSelectAndCaseClassSelectRemover.removeTopLevelTupleSelectsAndCaseClassSelects(leonListExpr)
    actualLeonListExpr match {
      case CaseClass(CaseClassType(`consCaseClassDef`, targs), args) =>
        args match {
          case List(elem, remainingList) => leon.collection.List(elem) ++ exprOfLeonListOfExprToLeonListOfExpr(remainingList)
        }
      case CaseClass(CaseClassType(`nilCaseClassDef`, targs), args) => leon.collection.List()
      case _ => throw new Exception("Did not match leon list expr")
    }
  }
}
