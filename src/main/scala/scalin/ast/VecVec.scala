package scalin
package ast

import spire.algebra._

object VecVec {

  abstract class Linear[A] extends AbstractVec[A] {

    def lhs: AbstractVec[A]
    def rhs: AbstractVec[A]

    require(lhs.length == rhs.length)

    def length: Int = lhs.length

    def nextNonZero(k: Int) =
      spire.math.min(lhs.nextNonZero(k), rhs.nextNonZero(k))

    def touch(node: AbstractNode) = lhs.touch(node).merge(rhs.touch(node))

  }

  case class Plus[A](lhs: AbstractVec[A], rhs: AbstractVec[A])(implicit A: AdditiveSemigroup[A]) extends Linear[A] {
    def apply(k: Int): A = A.plus(lhs(k), rhs(k))
  }

  case class Minus[A](lhs: AbstractVec[A], rhs: AbstractVec[A])(implicit A: AdditiveGroup[A]) extends Linear[A] {
    def apply(k: Int): A = A.minus(lhs(k), rhs(k))
  }

  case class Outer[A](lhs: AbstractVec[A], rhs: AbstractRowVec[A])(implicit A: MultiplicativeSemigroup[A]) extends AbstractMat[A] {
    def rows = lhs.length
    def cols = rhs.length
    def nextNonZeroInRow(r: Int, c: Int) = rhs.nextNonZero(c)
    def nextNonZeroInCol(r: Int, c: Int) = lhs.nextNonZero(r)
    def touch(node: AbstractNode) = (lhs.touch(node).multiIfNotClean).merge(rhs.touch(node).multiIfNotClean)
    def apply(r: Int, c: Int) = A.times(lhs(r), rhs(c))
  }

}