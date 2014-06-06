import leon.lang._
import leon.lang.synthesis._
import leon.collection._
import leon.annotation._

object Oracles {

  def genList(i: Int)(implicit o: Oracle[Boolean]): List[Int] =
    ?(Cons(i, genList(i+1)(o.left)), Nil[Int]())(o.right)

  def testList = {
    withOracle { implicit o: Oracle[Boolean] => {
      genList(0)
    } ensuring {
      _.size > 2
    }}
  }


  def branchA(implicit o: Oracle[Int]): Int = ???

  def branchB(implicit o: Oracle[Int]): Int = ???

  def testBranch = {
    withOracle { o: Oracle[Int] => {
      branchA(o.left) + branchB(o.right)
    } ensuring {
      _ == 42
    }}
  }

}


