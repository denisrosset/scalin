package scalin
package impl
package func

import spire.algebra.MultiplicativeMonoid

trait VecMultiplicativeMonoid[A, VA <: Vec[A]]
    extends scalin.algebra.VecMultiplicativeMonoid[A, VA]
    with scalin.impl.func.VecFactory[A, VA] {

  implicit def scalar: MultiplicativeMonoid[A]

  import spire.syntax.multiplicativeMonoid._

  // builder methods

  def ones(length: Int): VA =
    fill(length)(scalar.one)

  def times(lhs: A, rhs: Vec[A]): VA = pointwiseUnary(rhs)(lhs * _)

  def times(lhs: Vec[A], rhs: A): VA = pointwiseUnary(lhs)(_ * rhs)

  def pointwiseTimes(lhs: Vec[A], rhs: Vec[A]): VA = pointwiseBinary(lhs, rhs)(_ * _)

  def kron(lhs: Vec[A], rhs: Vec[A]): VA =
    tabulate(lhs.length * rhs.length) { i =>
      val ri = i % rhs.length
      val li = i / rhs.length
      lhs(li) * rhs(ri)
    }

  def product(lhs: Vec[A]): A = fold(lhs)(scalar.one)(scalar.times)

}
