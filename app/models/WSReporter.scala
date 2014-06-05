package leon.web
package models

import play.api.libs.iteratee._
import play.api.libs.json._
import play.api.libs.json.Json.toJson

import leon.Reporter
import leon.Settings

class WSReporter(channel: Concurrent.Channel[JsValue]) extends Reporter(Settings()) {
  def emit(msg: Message) = {
    val prefix = msg.severity match {
      case INFO => ""
      case WARNING => "Warning: "
      case ERROR => "Warning: "
      case FATAL => "Fatal: "
      case DEBUG(_) => "Debug: "
    }

    channel.push(toJson(Map("kind" -> "log", "message" -> (prefix + msg.msg.toString))))
  }
}
