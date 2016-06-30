package leon.web.websitebuilder
package memory

import programEvaluator.SourceMap

import scala.concurrent.Future
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import leon.LeonContext
import leon.web.websitebuilder.stringModification.ClarificationSession

/**
  * Created by dupriez on 2/25/16.
  */
object Memory {
  // _1 : ID of the computation request
  // _2 : ID of the
  private var _sourceMap :  (Int, Future[Option[SourceMap]], LeonContext)= null
  private var _autoSourceMap: (Int, Future[Option[SourceMap]], LeonContext) = null // For automatically generated code.
  def setSourceMap(id: Int, produceSourceMap: ()=>Option[SourceMap])(implicit ctx: LeonContext): Unit = {
    println("Setting SourceMap with id: "+id)
    if(_sourceMap != null && _sourceMap._3 != null) {
      _sourceMap._3.interruptManager.interrupt()
    }
    _sourceMap = (id, Future { produceSourceMap() }, ctx)
  }
  def setAutoSourceMap(id: Int, produceSourceMap: ()=>Option[SourceMap])(implicit ctx: LeonContext): Unit = {
    println("Setting autoSourceMap with id: "+id)
    if(_autoSourceMap != null && _autoSourceMap._3 != null) {
      _autoSourceMap._3.interruptManager.interrupt()
    }
    _autoSourceMap = (id, Future { produceSourceMap() }, ctx)
    println("AutoSourceMap has one for id: "+_autoSourceMap._1)
  }
  /** Returns the source map if the ids corresponds.*/
  def getSourceMap(id: Int): Option[SourceMap] = {
//    TODO: turn these println into serverReporter.report
    println("getSourceMap called for id "+id)
    if (_sourceMap == null) {
      throw new RuntimeException("Memory was asked for the sourceMap, while the sourceMap var contained a null")
    }
    else {
      if(id != _sourceMap._1) {
        println("Variable _sourceMap does not contain a sourceMap for id "+id+" (it contains a sourceMap for id "+_sourceMap._1+")")
        println("Looking in the _autoSourceMap variable")
        if(_autoSourceMap != null) {
          println("AutoSourceMap has one for id: "+_autoSourceMap._1)
          if(id != _autoSourceMap._1) {
            println("Variable _autoSourceMap does not contain a sourceMap for id "+id+" (it contains a sourceMap for id "+_autoSourceMap._1+")")
            None
          } else {
            try {
              Await.result(_autoSourceMap._2, 100.seconds)
            } catch {
              case e: Throwable =>
                println(e.getMessage)
                println(e.getStackTrace.mkString("\n"))
                None
            }
          }
        } else  None
      } else {
        println("Variable _sourceMap does contain a sourceMap for id "+id+". Awaiting the result of "+_sourceMap._2)
        try {
          Await.result(_sourceMap._2, 100.seconds)
        } catch {
          case e: Throwable => None
        }
      }
    }
  }
  def lastSourceId = _sourceMap._1

//  Keys are the ids of the StringModification that triggered the need for Ambiguity Resolving
  private var _clarificationSessionOption: Option[ClarificationSession] = None
  def clarificationSessionOption_=(newValue: Option[ClarificationSession]) = {
    println("Newly registered clarification session:")
    newValue match {
      case None => println("  None")
      case Some(clarificationSession) =>
        println("textElementIdsForEquations: "+clarificationSession.textElementIdsForEquations)
        println("idsOfInvolvedTextElements: "+ clarificationSession.idsOfInvolvedTextElements)
    }
    _clarificationSessionOption = newValue
  }
  def clarificationSessionOption = _clarificationSessionOption

  def reinitialiseSourceMapsVariablesAndClarificationSession() = {
    println("Reinitialising source map variables and clarification session")
    _sourceMap = null
    _autoSourceMap = null
    clarificationSessionOption = None
  }

  def clearClarificationSession() = {
    println("Clearing clarification session")
    clarificationSessionOption = None
  }
}
