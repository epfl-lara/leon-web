package leon.web
package workers

import akka.actor._

import models._

import leon._
import leon.utils._

abstract class WorkerActor(val session: ActorRef, val interruptManager: InterruptManager)  extends BaseActor {
  import ConsoleProtocol._

  val reporter = new WorkerReporter(session)

  lazy implicit val ctx = LeonContext(reporter = reporter,
                                      interruptManager = interruptManager,
                                      options =
                                        Seq(
                                            LeonOption(GlobalOptions.optSelectedSolvers)(Set("smt-cvc4","smt-z3"))
                                        ))

  def pushMessage(v: Array[Byte]) = session ! NotifyClientBin(v)
}

