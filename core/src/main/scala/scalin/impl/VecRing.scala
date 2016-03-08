package scalin
package impl

import spire.algebra.{Eq, Ring}
import spire.syntax.ring._
import spire.syntax.cfor._

trait VecRing[A, VA <: Vec[A]]
    extends scalin.algebra.VecRing[A, VA]
    with scalin.impl.VecMultiplicativeMonoid[A, VA] {

  //// Creation

  def zeros(length: Int): VA = fillConstant(length)(scalar.zero)

  //// Additive group methods

  def plus(lhs: Vec[A], rhs: Vec[A]): VA = pointwiseBinary(lhs, rhs)(_ + _)

  def negate(lhs: Vec[A]): VA = pointwiseUnary(lhs)(-_)

  def minus(lhs: Vec[A], rhs: Vec[A]): VA = pointwiseBinary(lhs, rhs)(_ - _)

  def pointwisePlus(lhs: Vec[A], rhs: A): VA = pointwiseUnary(lhs)(_ + rhs)

  def pointwiseMinus(lhs: Vec[A], rhs: A): VA = pointwiseUnary(lhs)(_ - rhs)

  def sum(lhs: Vec[A]): A = fold(lhs)(scalar.zero)(scalar.plus)

  //// Ring methods

  def dot(lhs: Vec[A], rhs: Vec[A]): A = {
    val n = lhs.length
    require(n == rhs.length)
    if (n == 0)
      scalar.zero
    else {
      var sum = lhs(0) * rhs(0)
      cforRange(1 until n) { k =>
        sum += lhs(k) * rhs(k)
      }
      sum
    }
  }


  def nnz(lhs: Vec[A])(implicit ev: Eq[A]): Int = count(lhs)(scalar.isZero(_))

}
