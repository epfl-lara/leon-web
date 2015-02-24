import leon.lang._

/**
 * 1) Implement the isSearchTree property that checks bounds of elements in a
 *    search tree. Assume that the tree is strictly sorted (no dupplicates)
 * 2) Implement operations on Binary Search Trees as efficiently as you can.
 *    These operations will likely not verify, but make sure that leon does not
 *    find counter-examples within a reasonnable timeout (e.g. --timeout=5 )
 *
 *    You do not need to change the pre-/post-conditions
 */

object BinaryTree {
  abstract class Option[T] {
    def getOrElse(default: T) = this match {
      case Some(v) => v
      case None() => default
    }
  }
  case class Some[T](v: T) extends Option[T]
  case class None[T]() extends Option[T]


  abstract class Tree {
    def content: Set[BigInt] = {
      this match {
        case Empty => Set()
        case Node(l, v, r) => l.content ++ Set(v) ++ r.content
      }
    }

    def size: BigInt = {
      this match {
        case Empty => 0
        case Node(l, _, r) => l.size + r.size + 1
      }
    } ensuring { _ >= 0 }


    def +(x: BigInt): Tree = {
      require(isBT)
      this // TODO
    } ensuring {
      res => res.isBT &&
             res.content == this.content ++ Set(x) &&
             res.size >= this.size &&
             res.size <= this.size + 1
    }

    def -(x: BigInt): Tree = {
      require(isBT)
      this // TODO
    } ensuring {
      res => res.isBT &&
             res.content == this.content -- Set(x) &&
             res.size <= this.size &&
             res.size >= this.size - 1
    }

    def ++(that: Tree): Tree = {
      require(this.isBT && that.isBT)
      this // TODO
    } ensuring {
      res => res.isBT && res.content == this.content ++ that.content
    }

    def contains(x: BigInt): Boolean = {
      require(isBT)
      false // TODO
    } ensuring {
      res => res == (content contains x)
    }

    // Properties

    def isBT: Boolean = {
      isSearchTree(None(), None())
    }

    def isSearchTree(min: Option[BigInt], max: Option[BigInt]): Boolean = {
      this match {
        case Empty => true
        case Node(l, v, r) =>
          true // TODO
      }
    }
  }

  case object Empty extends Tree
  case class Node(l: Tree, v: BigInt, r: Tree) extends Tree

}

