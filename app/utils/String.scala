/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package utils

object String {

  implicit class StringOps(val s: String) extends AnyVal {
    def extension = s.substring(s.lastIndexOf(".") + 1)
  }

}

