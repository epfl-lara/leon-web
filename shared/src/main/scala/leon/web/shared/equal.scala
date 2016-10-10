package leon.web
package shared

import scala.language.implicitConversions
import scala.annotation.implicitNotFound

object equal {

  /** For type-safe equality, extends equal.EqSyntax, or create an object extending it and import its content.
    * By using the `===` or the `=!=` instead of `==` one gets
    * a compile-time error if the two values being compared are
    * not of equal types or are not in a subtyping relationshing.
    *
    * @url https://github.com/non/algebra/blob/master/core/src/main/scala/algebra/Eq.scala
    * @url https://hseeberger.github.io/blog/2013/05/31/implicits-unchained-type-safe-equality-part2/
    * @url https://hseeberger.github.io/blog/2013/06/01/implicits-unchained-type-safe-equality-part3/
    */
  @implicitNotFound("Eq requires ${A} and ${B} to be either equal or in a subtyping relationship")
  sealed trait Eq[A, B] {

    @inline
    def eqv(x: A, y: B): Boolean
  }

  /** Since all the Eq instances below delegate the actual
    * equality check to `Any`'s `==` method, we can avoid creating
    * a new instance each time, and re-use this one instead.
    */
  private object AnyEq extends Eq[Any, Any] {

    @inline
    override def eqv(x: Any, y: Any): Boolean = x == y

  }

  object Eq extends LowPriorityEqImplicits {

    @inline
    final implicit def leftEqualsRightEq[A]: Eq[A, A] =
      AnyEq.asInstanceOf[Eq[A, A]]

    @inline
    final implicit def rightSubtypeOfLeftEq[A, B <: A]: Eq[A, B] =
      AnyEq.asInstanceOf[Eq[A, B]]

  }

  trait LowPriorityEqImplicits {

    @inline
    final implicit def leftSubtypeOfRightEq[B, A <: B]: Eq[A, B] =
      AnyEq.asInstanceOf[Eq[A, B]]

  }

  trait EqSyntax {

    @inline
    implicit def eqOps[A](x: A): EqOps[A] =
      new EqOps(x)

  }

  final class EqOps[A](val x: A) extends AnyVal {

    @inline
    final def ===[B](y: B)(implicit E: Eq[A, B]): Boolean =
      E.eqv(x, y)

    @inline
    final def =!=[B](y: B)(implicit E: Eq[A, B]): Boolean =
      !E.eqv(x, y)

  }

}

