package scalin
package ast

import spire.algebra._

object Vec {

  abstract class Linear[A] extends AbstractVec[A] {

    def vec: AbstractVec[A]

    def nextNonZero(k: Int) = vec.nextNonZero(k)
    def length: Int = vec.length
    def touch(node: AbstractNode) = vec.touch(node)

  }

  case class Negate[A](vec: AbstractVec[A])(implicit A: AdditiveGroup[A]) extends Linear[A] {
    def apply(k: Int): A = A.negate(vec(k))
  }
  
  case class LeftScalarTimes[A](lhs: A, vec: AbstractVec[A])(implicit A: MultiplicativeSemigroup[A]) extends Linear[A] {
    def apply(k: Int): A = A.times(lhs, vec(k))
  }

  case class RightScalarTimes[A](vec: AbstractVec[A], rhs: A)(implicit A: MultiplicativeSemigroup[A]) extends Linear[A] {
    def apply(k: Int): A = A.times(vec(k), rhs)
  }

  case class RightScalarDiv[A](vec: AbstractVec[A], rhs: A)(implicit A: MultiplicativeGroup[A]) extends Linear[A] {
    def apply(k: Int): A = A.div(vec(k), rhs)
  }

  case class Conj[A](vec: AbstractVec[A])(implicit A: Involution[A]) extends Linear[A] {
    def apply(k: Int): A = A.dagger(vec(k))
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
