import leon.Utils._
object Numerals {
  sealed abstract class Num
  case object Z extends Num
  case class  S(pred: Num) extends Num

  def value(n:Num) : Int = {
    n match {
      case Z => 0
      case S(p) => 1 + value(p)
    }
  } ensuring (_ >= 0)

  def add(x: Num, y: Num): Num = {
    choose { (r : Num) =>
      value(r) == value(x) + value(y)
    }
  }
}
