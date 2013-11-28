package leon.web
package workers

import akka.actor._
import play.api.libs.json._
import play.api.libs.json.Json._

import models._

trait WorkerActor extends BaseActor {
  import ConsoleProtocol._

  def session: ActorRef

  def pushMessage(v: JsValue) = session ! NotifyClient(v)
}

