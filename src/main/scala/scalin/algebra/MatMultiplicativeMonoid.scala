package scalin
package algebra

import spire.algebra._

trait MatMultiplicativeMonoid[A, MA <: Mat[A]] extends MatTrait[A, MA] {

  implicit def scalar: MultiplicativeMonoid[A]

  import spire.syntax.multiplicativeMonoid._

  // builder methods

  def ones(rows: Int, cols: Int): MA =
    fill(rows, cols)(scalar.one)

  def times(lhs: A, rhs: Mat[A]): MA = pointwiseUnary(rhs)(lhs * _)

  def times(lhs: Mat[A], rhs: A): MA = pointwiseUnary(lhs)(_ * rhs)

  def pointwiseTimes(lhs: Mat[A], rhs: Mat[A]): MA = pointwiseBinary(lhs, rhs)(_ * _)

  def dyad(lhs: Vec[A], rhs: Vec[A]): MA = tabulate(lhs.length, rhs.length) { (r, c) => lhs(r) * rhs(c) }

  def kron(lhs: Mat[A], rhs: Mat[A]): MA =
    tabulate(lhs.rows * rhs.rows, lhs.cols * rhs.cols) { (r, c) =>
      val rr = r % rhs.rows
      val rl = r / rhs.rows
      val cr = c % rhs.cols
      val cl = c / rhs.cols
      lhs(rl, cl) * rhs(rr, cr)
    }

}
