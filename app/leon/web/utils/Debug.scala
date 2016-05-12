package leon.web
package utils

import play.api.Logger

case class Debug(logger: Logger) {

  def log(msg: String)(implicit line: sourcecode.Line, file: sourcecode.File): Unit = {
    println(s"${file.value}:${line.value} $msg")
  }

  def debug[V](value: sourcecode.Text[V])
              (implicit enclosing: sourcecode.Enclosing): Unit = {
    println(s"${enclosing.value}: ${value.value}")
  }

  def debugSource[V](value: sourcecode.Text[V])
              (implicit enclosing: sourcecode.Enclosing): Unit = {
    println(s"${enclosing.value} [${value.source}]: ${value.value}")
  }

  def apply[V](value: sourcecode.Text[V])
              (implicit enclosing: sourcecode.Enclosing): Unit = {
    debug(value)
  }

}

