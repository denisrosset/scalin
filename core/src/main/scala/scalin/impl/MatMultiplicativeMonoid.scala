package scalin
package impl

import spire.syntax.multiplicativeMonoid._

trait MatMultiplicativeMonoid[A, MA <: Mat[A]]
    extends scalin.algebra.MatMultiplicativeMonoid[A, MA]
    with scalin.impl.MatEngine[A, MA] {


  //// Creation

  def ones(rows: Int, cols: Int): MA = fillConstant(rows, cols)(scalar.one)

  //// With `MultiplicativeMonoid[A]`, returning scalar

  def product(lhs: Mat[A]): A = fold(lhs)(scalar.one)(scalar.times)

  //// With `MultiplicativeMonoid[A]`, returning matrix

  def times(lhs: A, rhs: Mat[A]): MA = pointwiseUnary(rhs)(lhs * _)

  def times(lhs: Mat[A], rhs: A): MA = pointwiseUnary(lhs)(_ * rhs)

  def pointwiseTimes(lhs: Mat[A], rhs: Mat[A]): MA = pointwiseBinary(lhs, rhs)(_ * _)

  def dyad(lhs: Vec[A], rhs: Vec[A]): MA = tabulate(lhs.length, rhs.length) { (r, c) => lhs(r) * rhs(c) }

}
