package leon.web.websitebuilder.programEvaluator.dustbin

import leon.purescala.Definitions.Program
import leon.purescala.Expressions.Expr
import leon.solvers.string.StringSolver.Assignment
import leon.web.websitebuilder.logging.OptionValWithLog
import leon.webDSL.webDescription.WebElement

/**
  * Created by dupriez on 3/31/16.
  */
class SourceMap(val sourceCode: String, program: Program) extends ProgramExtractor(program) {
  private val _webElementIDToExpr: scala.collection.mutable.Map[Int, (WebElement, Expr)] = scala.collection.mutable.Map()
  def webElementIDToExpr(webElementID: Int) : OptionValWithLog[Expr] = {
    if (_webElementIDToExpr.contains(webElementID)) {
      OptionValWithLog(Some(_webElementIDToExpr(webElementID)._2), "SourceMap query succesful")
    }
    else {
      OptionValWithLog(None, "SourceMap query for webElementID: \"" + webElementID + "\" failed")
    }
  }
  def webElementIDToWebElement(webElementID: Int) : OptionValWithLog[WebElement] = {
    if (_webElementIDToExpr.contains(webElementID)) {
      OptionValWithLog(Some(_webElementIDToExpr(webElementID)._1), "SourceMap query succesful")
    }
    else {
      OptionValWithLog(None, "SourceMap query for webElementID: \"" + webElementID + "\" failed")
    }
  }
  def addMapping(id: Int, webElement: WebElement, exprOfUneavaluatedWebElement: Expr) = {
    _webElementIDToExpr(id) = (webElement, exprOfUneavaluatedWebElement)
  }
  def keys = _webElementIDToExpr.keys
}


