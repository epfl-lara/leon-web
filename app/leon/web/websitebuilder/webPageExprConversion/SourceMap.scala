package leon.web.websitebuilder.webPageExprConversion

import leon.purescala.Expressions.Expr
import leon.web.websitebuilder.logging.serverReporter.{Info, ServerReporter}
import leon.webDSL.webDescription.WebElement

/**
  * Created by dupriez on 26/06/16.
  */

class SourceMap {
  private val _webElementIDToWebElementAndUnevaluatedExpr: scala.collection.mutable.Map[Int, (WebElement, Expr)] = scala.collection.mutable.Map()
  private def query(webElementID: Int, serverReporter: ServerReporter): Option[(WebElement, Expr)]= {
    if (_webElementIDToWebElementAndUnevaluatedExpr.contains(webElementID)) {
      serverReporter.addTab.report(Info, "SourceMap query successful for webElement id= "+webElementID)
      Some(_webElementIDToWebElementAndUnevaluatedExpr(webElementID))
    }
    else {
      serverReporter.addTab.report(Info, "SourceMap query failed for webElement id= "+webElementID)
      None
    }
  }

  def addMapping(webElementID: Int, webElement: WebElement, unevaluatedExprOfWebElement: Expr) = {
    _webElementIDToWebElementAndUnevaluatedExpr(webElementID) = (webElement, unevaluatedExprOfWebElement)
  }

  def keys = _webElementIDToWebElementAndUnevaluatedExpr.keys

  def getWebElementFromID(webElementID: Int, serverReporter: ServerReporter): Option[WebElement] = {
    query(webElementID, serverReporter) match {
      case Some(webElementAndUnevaluatedExpr) => Some(webElementAndUnevaluatedExpr._1)
      case None => None
    }
  }

  def getUnevaluatedExprFromID(webElementID: Int, serverReporter: ServerReporter): Option[Expr] = {
    query(webElementID, serverReporter) match {
      case Some(webElementAndUnevaluatedExpr) => Some(webElementAndUnevaluatedExpr._2)
      case None => None
    }
  }
}