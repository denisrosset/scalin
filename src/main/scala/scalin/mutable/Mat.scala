package scalin
package mutable

import spire.algebra._

trait Mat[A] extends scalin.Mat[A] with Mutable { lhs =>

  def update(r: Int, c: Int, a: A): Unit

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

  def :=(rhs: AbstractMat[A]): Unit

}

object Mat extends MatFactory[Mat, Dummy] {

  def tabulate[A:Dummy](rows: Int, cols: Int)( f: (Int, Int) => A ): Mat[A] =
    DenseMat.tabulate[A](rows, cols)(f)

}
