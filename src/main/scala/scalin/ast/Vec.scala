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

}
