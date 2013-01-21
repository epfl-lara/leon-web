package examples

object TutorialExamples {
  var allExamples = List[Example]()

  def newExample(title: String, code: String) {
    allExamples = allExamples ::: Example(title, code) :: Nil
  }

  def default = allExamples.head

  newExample("Introduction", """
import leon.Utils._

/**
 * Code should be contained in a single top-level object 
 */
object Introduction {

  /**
   * You can define Algebraic Data Types using abstract classes and case classes.
   * Note that Leon does not support "case object Foo", so you will need "case
   * class Foo()"
   */

  case class Person(age: Int)

  /**
   * You can define functions. Functions should be purely functionnal. Note
   * that some imperative constructs such as 'var' or while loops will be handled.
   */
  def isAdult(p: Person) : Boolean = {
    p.age > 18
  }
  
  def incAge(p: Person): Person = {
      Person(p.age+1)
  }
  
  /**
   * You can specify pre-conditions to a function using the "require" construct.
   * You can use previously-defined functions in your precondition
   */
  def incAgePre(p: Person): Person = {
      require(isAdult(p))
      Person(p.age+1)
  }
    
  /**
   * You can specify post-conditions using "ensuring".
   * 
   * Functions with post-conditions become "verifiable": a little "v" will 
   * appear in the margin. Click on the function name and select "Verify" 
   * to ask Leon if the post-condition is always satisfied.
   */
  def incAgePostInvalid(p: Person): Person = {
      Person(p.age+1)
  } ensuring { res => isAdult(res) }
  
      
  /**
   * With the additional pre-condition, this becomes valid:
   */
  def incAgePostValid(p: Person): Person = {
      require(isAdult(p))
      Person(p.age+1)
  } ensuring { res => isAdult(res) }
  
}
""".trim)

  newExample("Account", """
import leon.Utils._

object Account2 {
  sealed abstract class AccLike
  case class Acc(checking : Int, savings : Int) extends AccLike

  /**
   * Exercice 1:
   * 
   *  Complete the precondition such that the function verifies.
   *  You can try to verify it as-is. The counter-example you will get can be
   *  helpful.
   */
  def toSavings(x : Int, a : Acc) : Acc = {
    require (notRed(a) && a.checking >= x)
    Acc(a.checking - x, a.savings + x)
  } ensuring (res => (notRed(res) && sameTotal(a, res)))


  def sameTotal(a1 : Acc, a2 : Acc) : Boolean = {
    a1.checking + a1.savings == a2.checking + a2.savings
  }

  def notRed(a : Acc) : Boolean = {
    a.checking >= 0 && a.savings >= 0
  }
}
""".trim)

  newExample("Insertion Sort", """
import scala.collection.immutable.Set
import leon.Annotations._
import leon.Utils._

object InsertionSort {
  sealed abstract class List
  case class Cons(head:Int,tail:List) extends List
  case class Nil() extends List

  sealed abstract class OptInt
  case class Some(value: Int) extends OptInt
  case class None() extends OptInt

  def size(l : List) : Int = (l match {
    case Nil() => 0
    case Cons(_, xs) => 1 + size(xs)
  }) 

  def contents(l: List): Set[Int] = l match {
    case Nil() => Set.empty
    case Cons(x,xs) => contents(xs) ++ Set(x)
  }

  def min(l : List) : OptInt = l match {
    case Nil() => None()
    case Cons(x, xs) => min(xs) match {
      case None() => Some(x)
      case Some(x2) => if(x < x2) Some(x) else Some(x2)
    }
  }

  def isSorted(l: List): Boolean = l match {
    case Nil() => true
    case Cons(x, Nil()) => true
    case Cons(x, Cons(y, ys)) => x <= y && isSorted(Cons(y, ys))
  }   

  /* Inserting element 'e' into a sorted list 'l' produces a sorted list with
   * the expected content and size */
  def sortedIns(e: Int, l: List): List = {
    require(isSorted(l))
    l match {
      case Nil() => Cons(e,Nil())
      case Cons(x,xs) => if (x <= e) Cons(x,sortedIns(e, xs)) else Cons(e, l)
    } 
  } 

 
  /* Insertion sort yields a sorted list of same size and content as the input
   * list */
  def sort(l: List): List = (l match {
    case Nil() => Nil()
    case Cons(x,xs) => sortedIns(x, sort(xs))
  }) ensuring(res => contents(res) == contents(l) 
                     && isSorted(res)
                     && size(res) == size(l)
             )

}
""".trim)

  newExample("List", """
import scala.collection.immutable.Set
import leon.Annotations._
import leon.Utils._

object ListWithSize {
    sealed abstract class List
    case class Cons(head: Int, tail: List) extends List
    case class Nil() extends List

    sealed abstract class IntPairList
    case class IPCons(head: IntPair, tail: IntPairList) extends IntPairList
    case class IPNil() extends IntPairList

    sealed abstract class IntPair
    case class IP(fst: Int, snd: Int) extends IntPair

    
    def size(l: List) : Int = (l match {
        case Nil() => 0
        case Cons(_, t) => 1 + size(t)
    })

    def iplSize(l: IntPairList) : Int = (l match {
      case IPNil() => 0
      case IPCons(_, xs) => 1 + iplSize(xs)
    })

    // Verify
    def zip(l1: List, l2: List) : IntPairList = {
      l1 match {
        case Nil() => IPNil()
        case Cons(x, xs) => l2 match {
          case Cons(y, ys) => IPCons(IP(x, y), zip(xs, ys))
        }
      }
    } ensuring(iplSize(_) == size(l1))

    def sizeTailRec(l: List) : Int = sizeTailRecAcc(l, 0)
    def sizeTailRecAcc(l: List, acc: Int) : Int = {
     l match {
       case Nil() => acc
       case Cons(_, xs) => sizeTailRecAcc(xs, acc+1)
     }
    } 

    // Verify
    def sizesAreEquiv(l: List) : Boolean = {
      size(l) == sizeTailRec(l)
    } holds

    def content(l: List) : Set[Int] = l match {
      case Nil() => Set.empty[Int]
      case Cons(x, xs) => Set(x) ++ content(xs)
    }

    def sizeAndContent(l: List) : Boolean = {
      size(l) == 0 || content(l) != Set.empty[Int]
    } holds
    
    def drunk(l : List) : List = (l match {
      case Nil() => Nil()
      case Cons(x,l1) => Cons(x,Cons(x,drunk(l1)))
    }) // TODO: find postcondition

    
    def funnyCons(x: Int, l: List) : List = (l match {
        case Nil() => Cons(x, Nil())
        case c @ Cons(_,_) => Cons(x, c)
    }) ensuring(size(_) > 0)

    // Verify
    def reverse(l: List) : List = reverse0(l, Nil()) ensuring(content(_) == content(l))
    def reverse0(l1: List, l2: List) : List = (l1 match {
      case Nil() => l2
      case Cons(x, xs) => reverse0(xs, Cons(x, l2))
    }) 

    def append(l1 : List, l2 : List) : List = (l1 match {
      case Nil() => l2
      case Cons(x,xs) => Cons(x, append(xs, l2))
    }) ensuring(content(_) == content(l1) ++ content(l2))

    @induct
    def nilAppend(l : List) : Boolean = (append(l, Nil()) == l) holds

    // TODO: find predicate
    //@induct
    //def appendAssoc(xs : List, ys : List, zs : List) : Boolean =
    //  (...) holds

    @induct
    def sizeAppend(l1 : List, l2 : List) : Boolean =
      (size(append(l1, l2)) == size(l1) + size(l2)) holds

    @induct
    def concat(l1: List, l2: List) : List = 
      concat0(l1, l2, Nil()) ensuring(content(_) == content(l1) ++ content(l2))

    @induct
    def concat0(l1: List, l2: List, l3: List) : List = (l1 match {
      case Nil() => l2 match {
        case Nil() => reverse(l2)
        case Cons(y, ys) => {
          concat0(Nil(), ys, Cons(y, l3))
        }
      }
      case Cons(x, xs) => concat0(xs, l2, Cons(x, l3))
    }) ensuring(content(_) == content(l1) ++ content(l2) ++ content(l3))

}
""".trim)
  
  newExample("Propositional Logic", """
import scala.collection.immutable.Set
import leon.Utils._
import leon.Annotations._

object PropositionalLogic {
  sealed abstract class Formula
  case class And(lhs: Formula, rhs: Formula) extends Formula
  case class Or(lhs: Formula, rhs: Formula) extends Formula
  case class Implies(lhs: Formula, rhs: Formula) extends Formula
  case class Not(f: Formula) extends Formula
  case class Literal(id: Int) extends Formula


  def simplify(f: Formula): Formula = (f match {
    case And(lhs, rhs) => And(simplify(lhs), simplify(rhs))
    case Or(lhs, rhs) => Or(simplify(lhs), simplify(rhs))
    case Implies(lhs, rhs) => Or(Not(simplify(lhs)), simplify(rhs))
    case Not(f) => Not(simplify(f))
    case Literal(_) => f
  }) ensuring(isSimplified(_))

  def isSimplified(f: Formula): Boolean = f match {

  }

  def nnf(formula: Formula): Formula = (formula match {
    case And(lhs, rhs) => And(nnf(lhs), nnf(rhs))
    case Or(lhs, rhs) => Or(nnf(lhs), nnf(rhs))
    case Implies(lhs, rhs) => Implies(nnf(lhs), nnf(rhs))
    case Not(And(lhs, rhs)) => Or(nnf(Not(lhs)), nnf(Not(rhs)))
    case Not(Or(lhs, rhs)) => And(nnf(Not(lhs)), nnf(Not(rhs)))
    case Not(Implies(lhs, rhs)) => And(nnf(lhs), nnf(Not(rhs)))
    case Not(Not(f)) => nnf(f)
    case Not(Literal(_)) => formula
    case Literal(_) => formula
  }) ensuring(isNNF(_))

  def isNNF(f: Formula): Boolean = f match {

  }

  def evalLit(id : Int) : Boolean = (id == 42) // could be any function
  def eval(f: Formula) : Boolean = f match {
    case And(lhs, rhs) => eval(lhs) && eval(rhs)
    case Or(lhs, rhs) => eval(lhs) || eval(rhs)
    case Implies(lhs, rhs) => !eval(lhs) || eval(rhs)
    case Not(f) => !eval(f)
    case Literal(id) => evalLit(id)
  }

  @induct
  def simplifySemantics(f: Formula) : Boolean = {
    eval(f) == eval(simplify(f))
  } holds

  // Note that matching is exhaustive due to precondition.
  def vars(f: Formula): Set[Int] = {
    require(isNNF(f))
    f match {
      case And(lhs, rhs) => vars(lhs) ++ vars(rhs)
      case Or(lhs, rhs) => vars(lhs) ++ vars(rhs)
      case Implies(lhs, rhs) => vars(lhs) ++ vars(rhs)
      case Not(Literal(i)) => Set[Int](i)
      case Literal(i) => Set[Int](i)
    }
  }
}
""".trim)

}
