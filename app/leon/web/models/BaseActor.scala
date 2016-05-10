package leon.web
package models

import akka.actor._
import play.api._
import play.api.libs.json._
import play.api.libs.json.Json._
import leon.web.shared.messages._
import leon.web.shared.messages.Picklers._
import java.nio.ByteBuffer
import boopickle.Default._

trait BaseActor extends Actor {

  def pushMessage(v: Array[Byte]): Unit

  def notifySuccess(msg: String): Unit = {
    event(HNotification(msg, "success"))
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
    event(HLog(msg))
  }

  def event(msg: Message): Unit = {
    pushMessage(Pickle.intoBytes(msg).array())
  }


  def notifyError(msg: String): Unit = {
    event(HNotification(msg, "error"))
  }

}
