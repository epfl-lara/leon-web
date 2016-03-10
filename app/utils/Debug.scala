package leon.web
package utils

object Debug {

  def apply[V](value: sourcecode.Text[V])(implicit enclosing: sourcecode.Enclosing) = {
    println(enclosing.value + " [" + value.source + "]: " + value.value + "\n")
  }

}

