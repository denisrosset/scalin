package scalin
package impl

import spire.algebra.MultiplicativeMonoid

trait VecMultiplicativeMonoid[A, VA <: Vec[A]]
    extends scalin.algebra.VecMultiplicativeMonoid[A, VA]
    with scalin.impl.VecEngine[A, VA] {

  import spire.syntax.multiplicativeMonoid._

  //// Creation

  def ones(length: Int): VA = fillConstant(length)(scalar.one)

  //// With `MultiplicativeMonoid[A]`, returning scalar

  def product(lhs: Vec[A]): A = fold(lhs)(scalar.one)(scalar.times)

  //// With `MultiplicativeMonoid[A]`, returning vector

  def times(lhs: A, rhs: Vec[A]): VA = pointwiseUnary(rhs)(lhs * _)

  def times(lhs: Vec[A], rhs: A): VA = pointwiseUnary(lhs)(_ * rhs)

  def pointwiseTimes(lhs: Vec[A], rhs: Vec[A]): VA = pointwiseBinary(lhs, rhs)(_ * _)

}
