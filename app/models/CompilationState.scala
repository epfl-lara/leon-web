package leon.web
package models

import leon.purescala.Common._
import leon.purescala.Definitions._

case class CompilationState (
  code: Option[String],
  compResult: String,
  optProgram: Option[Program],
  // Imperative information
  wasLoop: Set[FunDef]) {

  def program: Program = {
    optProgram.get
  }

  def isCompiled = optProgram.isDefined

  lazy val toInner = program.definedFunctions.filter(_.parent.isDefined).groupBy(_.parent.get)
  def innerFunctionsOf(fd: FunDef): Set[FunDef] = {
    toInner.getOrElse(fd, Set()).toSet
  }

  def functionWasLoop(fd: FunDef): Boolean =
    wasLoop.contains(fd.orig.getOrElse(fd))

  def filterFunction(fd: FunDef): Boolean = {
    !(fd.annotations contains "verified")
  }

  def functions = {
    program.definedFunctions.toList.filter(filterFunction).sortWith(_.getPos < _.getPos)
  }

}


object CompilationState {
  def failure(code: String) =
    CompilationState(Some(code), "failure", None, Set())

  def unknown = 
    CompilationState(None, "unknown", None, Set())
}
