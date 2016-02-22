package scalin
package ast

import spire.algebra._

object VecVec {

  case class Linear[A](lhs: AbstractVec[A], rhs: AbstractVec[A], f: (A, A) => A) extends AbstractVec[A] {

    require(lhs.length == rhs.length)

    def length: Int = lhs.length

    def nextNonZero(k: Int) =
      spire.math.min(lhs.nextNonZero(k), rhs.nextNonZero(k))

    def touch(node: AbstractNode) = lhs.touch(node).merge(rhs.touch(node))

    def apply(k: Int): A = f(lhs(k), rhs(k))

  }

  case class ElementwiseProduct[A](lhs: AbstractVec[A], rhs: AbstractVec[A], f: (A, A) => A) extends AbstractVec[A] {

    require(lhs.length == rhs.length)

    def length: Int = lhs.length

    def nextNonZero(k: Int) =
      spire.math.max(lhs.nextNonZero(k), rhs.nextNonZero(k))

    def touch(node: AbstractNode) = lhs.touch(node).merge(rhs.touch(node))

    def apply(k: Int): A = f(lhs(k), rhs(k))

  }

  case class Elementwise[A, B](lhs: AbstractVec[A], rhs: AbstractVec[A], f: (A, A) => B) extends AbstractVec[B] {

    require(lhs.length == rhs.length)

    def length: Int = lhs.length

    def nextNonZero(k: Int) = k + 1

    def touch(node: AbstractNode) = lhs.touch(node).merge(rhs.touch(node))

    def apply(k: Int) = f(lhs(k), rhs(k))

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
