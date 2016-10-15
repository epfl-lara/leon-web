package leon.web
package workers

import akka.actor._

import models._

import leon._
import leon.utils._

abstract class WorkerActor(val session: ActorRef, val interruptManager: InterruptManager)  extends BaseActor {
  import ConsoleProtocol._

  val reporter = new WorkerReporter(session)

  private var _ctx: LeonContext = mergeWithOptions(Nil)
  
  def mergeWithOptions(customOptions: Seq[String]): LeonContext = {
    val originalContext = leon.Main.processOptions(List(
      "--feelinglucky",
      "--solvers=smt-cvc4,smt-z3,ground",
      "--debug=verification,solver"
    ))
    val augmentedContext = if(customOptions == Nil) {
      originalContext
    } else {
      originalContext ++ leon.Main.parseOptions(customOptions, false)
    }
    augmentedContext.copy(interruptManager = interruptManager, reporter = reporter)
  }
  
  def applyLeonContextOptions(customOptions: Seq[String], overridingOptions: Seq[String]*): Unit = {
    _ctx = (mergeWithOptions(customOptions) /: overridingOptions) {
      case (lctx, seqString) => lctx ++ leon.Main.parseOptions(seqString, true)
    }
  }

  implicit def ctx = _ctx                   

  def pushMessage(v: Array[Byte]) = session ! NotifyClientBin(v)
}

