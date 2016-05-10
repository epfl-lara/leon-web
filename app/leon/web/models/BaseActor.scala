package leon.web
package models

import akka.actor._
import play.api._
import play.api.libs.json._
import play.api.libs.json.Json._

trait BaseActor extends Actor {

  def pushMessage(v: JsValue): Unit

  def notifySuccess(msg: String): Unit = {
    event("notification", Map(
      "content" -> toJson(msg),
      "type"    -> toJson("success")))
  }

  def logInfo(msg: String): Unit = {
    println(msg)
  }

  def logInfo(msg: String, t: Throwable): Unit = {
    Logger.info(msg)
    t.printStackTrace()
  }

  def clientLog(msg: String): Unit = {
    logInfo("[>] L: "+msg)
    pushMessage(toJson(Map("kind" -> "log", "level" -> "log", "message" -> msg)))
  }

  def event(kind: String, data: Map[String, JsValue]): Unit = {
    logInfo("[>] "+kind)
    pushMessage(toJson(Map("kind" -> toJson(kind)) ++ data))
  }


  def notifyError(msg: String): Unit = {
    event("notification", Map(
      "content" -> toJson(msg),
      "type"    -> toJson("error")))
  }

}
