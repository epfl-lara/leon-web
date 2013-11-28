package leon.web
package models

import leon.purescala.Definitions._
import leon.purescala.EquivalencePrettyPrinter

case class FunctionHash(fd: FunDef) {
  val v = {
    val pp = new EquivalencePrettyPrinter()
    pp.pp(fd)(0)
    pp.toString
  }

  override def equals(o: Any): Boolean = {
    o match {
      case fh: FunctionHash =>
        fh.v == v
      case _ =>
        false
    }
  }

  override def hashCode: Int = {
    v.##
  }
}

