package leon.web
package models

import scala.Function.const

import leon.web.utils.String._
import leon.web.shared.RepositoryState
import leon.purescala.Definitions._

case class CompilationState (
  code: Option[String],
  repoState: Option[RepositoryState] = None,
  savedFile: Option[String] = None,
  compResult: String,
  optProgram: Option[Program],
  requestId: Option[Int],
  // Imperative information
  wasLoop: Set[FunDef]) {

  def program: Program = {
    optProgram.get
  }

  def isCompiled = optProgram.isDefined

 // lazy val toInner = program.definedFunctions.filter(_.owner.isDefined).groupBy(_.owner.get)

  def innerFunctionsOf(fd: FunDef): Set[FunDef] = {
    Set()
    //toInner.getOrElse(fd, Set()).toSet
  }

  def functionWasLoop(fd: FunDef): Boolean = {
    fd.flags.exists {
      case IsLoop(_) => true
      case _ => false
    }
  }

  def filterFunction(fd: FunDef): Boolean = {
    !(fd.annotations contains "library")
  }

  def functions = repoState.flatMap(const(savedFile)) match {
    case None =>
      program.definedFunctions
        .toList
        .filter(filterFunction)
        .sortWith(_.getPos < _.getPos)

    case Some(file) =>
      functionsInUnit(file.fileName)
  }

  def functionsInUnit(unit: String) = {
    program.units
      .filter(_.id.name === unit)
      .flatMap(_.definedFunctions)
      .toList
      .filter(filterFunction)
      .sortWith(_.getPos < _.getPos)
  }

}


object CompilationState {

  def failure(code: String, repoState: Option[RepositoryState] = None, savedFile: Option[String] = None) =
    CompilationState(
      code       = Some(code),
      compResult = "failure",
      optProgram = None,
      requestId  = None,
      wasLoop    = Set(),
      repoState  = repoState,
      savedFile  = savedFile
    )

  def unknown =
    CompilationState(
      code       = None,
      compResult = "unknown",
      optProgram = None,
      requestId  = None,
      wasLoop    = Set(),
      repoState  = None,
      savedFile  = None
    )

}

