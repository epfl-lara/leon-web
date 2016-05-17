package leon.web
package models

import leon.LeonContext
import leon.purescala.Expressions._
import leon.purescala.Definitions._

trait StringToExprCached {
  implicit val ctx: LeonContext;
  
  protected def program: Option[Program]
  
  /** Caches expr to string transformation to revert them back. */
  protected var activateCache: Boolean = false
  def updateExprCache(s: String, e: Expr) = if(activateCache) exprCache += s -> e
  def getExprFromCache(s: String): Option[Expr] = if(activateCache) exprCache.get(s) else None
  def clearExprCache() = exprCache = Map()
  protected var exprCache = Map[String, Expr]()
}
