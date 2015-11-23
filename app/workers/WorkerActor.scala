package leon.web
package workers

import akka.actor._
import play.api.libs.json._

import models._

import leon._
import leon.utils._

abstract class WorkerActor(val session: ActorRef, val interruptManager: InterruptManager)  extends BaseActor {
  import ConsoleProtocol._

  val reporter = new WorkerReporter(session)

  lazy implicit val ctx = LeonContext(reporter = reporter,
                                      interruptManager = interruptManager)

  def pushMessage(v: JsValue) = session ! NotifyClient(v)
}

