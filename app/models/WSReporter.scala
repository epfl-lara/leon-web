package leon.web
package models

import play.api.libs.iteratee._
import play.api.libs.json._
import play.api.libs.json.Json.toJson
import leon.Reporter

class WSReporter(channel: PushEnumerator[JsValue]) extends Reporter {
  def infoFunction(msg: Any) : Unit = {
    channel.push(toJson(Map("kind" -> "log", "message" -> (msg.toString))))
  }

  def warningFunction(msg: Any) : Unit = {
    channel.push(toJson(Map("kind" -> "log", "message" -> ("Warning: "+msg.toString))))
  }

  def errorFunction(msg: Any) : Unit = {
    channel.push(toJson(Map("kind" -> "log", "message" -> ("Error: "+msg.toString))))
  }

  def fatalErrorFunction(msg: Any) : Nothing = {
    sys.error("FATAL: "+msg)
  }
}
