package leon.web
package models

import play.api.libs.iteratee._

class CompilingWSReporter(channel: Concurrent.Channel[Array[Byte]]) extends WSReporter(channel) {
  var errors   = Map[Int, Seq[String]]();
  var warnings = Map[Int, Seq[String]]();
  var infos    = Map[Int, Seq[String]]();

  val colIndicator = """(\s*\^\s*)""".r

  def extractMessage(msg: Message, to: Map[Int, Seq[String]]) : Map[Int, Seq[String]] = {

    val line = msg.position.line

    val parts = msg.toString.split(":", 2).toList

    val msgLines = msg.msg.toString.split("\n").toList.filter { _ match {
      case colIndicator(_) => false
      case _ => true
    }}

    to + (line -> (to.getOrElse(line, Nil) :+ msgLines.mkString("\n")))
  }

  override def onCompilerProgress(current: Int, total: Int): Unit = {
    import boopickle.Default._
    import shared.messages.{Message => HMessage, HCompilationProgress}
    import shared.messages.Picklers._
    channel.push(Pickle.intoBytes[HMessage](HCompilationProgress(current, total)).array())
  }

  override def account(msg: Message): Message = {
    msg.severity match {
      case ERROR =>
        errors = extractMessage(msg, errors)
      case WARNING =>
        warnings = extractMessage(msg, warnings)
      case INFO =>
        infos = extractMessage(msg, infos)
      case _ =>
    }
    super.account(msg)
  }
}
