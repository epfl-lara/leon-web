import leon.Annotations._
import leon.Utils._

object Complete {
  sealed abstract class List
  case class Cons(head: Int, tail: List) extends List
  case object Nil extends List

  def size(l: List) : Int = (l match {
      case Nil => 0
      case Cons(_, t) => 1 + size(t)
  }) ensuring(res => res >= 0)

  def content(l: List): Set[Int] = l match {
    case Nil => Set.empty[Int]
    case Cons(i, t) => Set(i) ++ content(t)
  }

  def isSorted(list : List) : Boolean = list match {
    case Nil => true
    case Cons(_, Nil) => true
    case Cons(x1, Cons(x2, _)) if(x1 > x2) => false
    case Cons(_, xs) => isSorted(xs)
  }

  def union(in1: List, in2: List): List = {
    require(isSorted(in1) && isSorted(in2))
    (in1, in2) match {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        if (h1 < h2) {
          Cons(h1, union(t1, in2))
        } else {
          Cons(h2, union(in1, t2))
        }
      case (Nil, l2) =>
        l2
      case (l1, Nil) =>
        l1
    }
  } ensuring { res => content(res) == content(in1) ++ content(in2) && isSorted(res) }

  // ***********************
  // Sorting algorithms
  // ***********************

  def splitSpec(list : List, res : (List,List)) : Boolean = {
    val s1 = size(res._1)
    val s2 = size(res._2)
    abs(s1 - s2) <= 1 && s1 + s2 == size(list) &&
    content(res._1) ++ content(res._2) == content(list) 
  }

  def abs(i : Int) : Int = {
    if(i < 0) -i else i
  } ensuring(_ >= 0)

  def sortSpec(in : List, out : List) : Boolean = {
    content(out) == content(in) && isSorted(out)
  }

  def split(list : List) : (List,List) = (list match {
    case Nil => (Nil, Nil)
    case Cons(x, Nil) => (Cons(x, Nil), Nil)
    case Cons(x1, Cons(x2, xs)) =>
      val (s1,s2) = split(xs)
      (Cons(x1, s1), Cons(x2, s2))
  }) ensuring(res => splitSpec(list, res))

  // This fails to synthesize due to a timeout inherent to the web-interface
  def sort(list: List): List = choose {
    (res: List) => sortSpec(list, res)
  }

}
