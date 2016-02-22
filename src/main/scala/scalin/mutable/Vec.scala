package scalin
package mutable

import spire.algebra._

trait Vec[A] extends scalin.Vec[A] with Mutable { lhs =>

  def update(k: Int, a: A): Unit
/*
  def +=(rhs: AbstractMat[A])(implicit A: AdditiveSemigroup[A]): Unit = {
    lhs := lhs + rhs
  }

  def -=(rhs: AbstractMat[A])(implicit A: AdditiveGroup[A]): Unit = {
    lhs := lhs - rhs
  }

  def *=(rhs: AbstractMat[A])(implicit A: Semiring[A]): Unit = {
    lhs := lhs * rhs
  }

  def :*=(rhs: A)(implicit A: MultiplicativeSemigroup[A]): Unit = {
    lhs := lhs :* rhs
  }
 */
  def :=(rhs: AbstractVec[A]): Unit

}

object Vec extends VecFactory[Vec, Dummy] {

  def tabulate[A:Dummy](length: Int)( f: Int => A ): Vec[A] =
    DenseVec.tabulate[A](length)(f)

}
