package leon.web
package models

import play.api.libs.iteratee._
import play.api.libs.json._
import play.api.libs.json.Json.toJson

class CompilingWSReporter(channel: Concurrent.Channel[JsValue]) extends WSReporter(channel) {
  var errors   = Map[Int, Seq[String]]();
  var warnings = Map[Int, Seq[String]]();
  var infos    = Map[Int, Seq[String]]();

  val colIndicator = """(\s*\^\s*)""".r

  def extractMessage(msg: Any, to: Map[Int, Seq[String]]) : Map[Int, Seq[String]] = {
    val parts = msg.toString.split(":", 2).toList

    parts match {
      case List(l, a) => 
        try {
          val line = l.toInt

          val msgLines = a.split("\n").toList.filter { _ match {
            case colIndicator(_) => false
            case _ => true
          }}

          to + (line -> (to.getOrElse(line, Nil) :+ msgLines.mkString("\n")))
        } catch {
          case t: Throwable =>
            to
        }
      case _ =>
        to
    }
  }

  override def onCompilerProgress(current: Int, total: Int) {
    channel.push(toJson(Map("kind" -> toJson("compilation_progress"),
                            "current" -> toJson(current),
                            "total" -> toJson(total))))
  }

  override def account(msg: Message): Message = {
    msg.severity match {
      case ERROR =>
        errors = extractMessage(msg.msg, errors)
      case WARNING =>
        warnings = extractMessage(msg.msg, warnings)
      case INFO =>
        infos = extractMessage(msg.msg, infos)
      case _ =>
    }
    super.account(msg)
  }
}
