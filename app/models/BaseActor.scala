package leon.web
package models

import akka.actor._
import play.api._
import play.api.libs.json._
import play.api.libs.json.Json._

import leon.purescala.Common._
import leon.purescala.Definitions._

trait BaseActor extends Actor {

  def pushMessage(v: JsValue)

  def notifySuccess(msg: String) = {
    event("notification", Map(
      "content" -> toJson(msg),
      "type"    -> toJson("success")))
  }

  def logInfo(msg: String) {
    Logger.info(msg)
  }

  def logInfo(msg: String, t: Throwable) {
    Logger.info(msg)
    t.printStackTrace()
  }

  def clientLog(msg: String) = {
    logInfo("[>] L: "+msg)
    pushMessage(toJson(Map("kind" -> "log", "level" -> "log", "message" -> msg)))
  }

  def event(kind: String, data: Map[String, JsValue]) = {
    logInfo("[>] "+kind)
    pushMessage(toJson(Map("kind" -> toJson(kind)) ++ data))
  }


  def notifyError(msg: String) = {
    event("notification", Map(
      "content" -> toJson(msg),
      "type"    -> toJson("error")))
  }

}
