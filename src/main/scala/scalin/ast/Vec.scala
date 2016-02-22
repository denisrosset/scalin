package scalin
package ast

import spire.algebra._

object Vec {

  case class Linear[A](vec: AbstractVec[A], f: A => A) extends AbstractVec[A] {

    def nextNonZero(k: Int) = vec.nextNonZero(k)
    def length = vec.length
    def touch(node: AbstractNode) = vec.touch(node)
    def apply(k: Int) = f(vec(k))

  }

  case class Elementwise[A, B](vec: AbstractVec[A], f: A => B) extends AbstractVec[B] {

    def length = vec.length

    def nextNonZero(k: Int) = k + 1

    def touch(node: AbstractNode) = vec.touch(node)

    def apply(k: Int) = f(vec(k))

  }


  case class ColT[A](col: AbstractVec[A]) extends AbstractRowVec[A] { lhs =>

    def apply(k: Int) = col(k)
    def nextNonZero(k: Int) = col.nextNonZero(k)
    def length = col.length
    def touch(node: AbstractNode) = col.touch(node).multiIfNotClean

  }

  case class ColDagger[A](col: AbstractVec[A])(implicit A: Involution[A]) extends AbstractRowVec[A] { lhs =>

    def apply(k: Int) = A.dagger(col(k))
    def nextNonZero(k: Int) = col.nextNonZero(k)
    def length = col.length
    def touch(node: AbstractNode) = col.touch(node).multiIfNotClean

  }

  case class RowT[A](row: AbstractRowVec[A]) extends AbstractVec[A] { lhs =>

    def apply(k: Int) = row(k)
    def nextNonZero(k: Int) = row.nextNonZero(k)
    def length = row.length
    def touch(node: AbstractNode) = row.touch(node).multiIfNotClean

  }

  case class RowDagger[A](row: AbstractRowVec[A])(implicit A: Involution[A]) extends AbstractVec[A] { lhs =>

    def apply(k: Int) = A.dagger(row(k))
    def nextNonZero(k: Int) = row.nextNonZero(k)
    def length = row.length
    def touch(node: AbstractNode) = row.touch(node).multiIfNotClean

  }

}
