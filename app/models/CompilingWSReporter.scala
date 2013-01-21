package leon.web
package models

import play.api.libs.iteratee._
import play.api.libs.json._
import play.api.libs.json.Json.toJson

class CompilingWSReporter(channel: PushEnumerator[JsValue]) extends WSReporter(channel) {
  var errors = Map[Int, Seq[String]]();

  override def infoFunction(msg: Any) : Unit = {
    val parts = msg.toString.split(":", 2).toList

    parts match {
      case List(l, a) => 
        try {
          val line = l.toInt

          errors += line -> (errors.getOrElse(line, Nil) :+ msg.toString)
        } catch {
          case t: Throwable =>
        }
      case _ =>
    }


    super.infoFunction(msg)
  }

}
