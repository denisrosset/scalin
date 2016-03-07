package scalin
package algebra

import spire.algebra._

trait MatMultiplicativeMonoid[A, M[A] <: Mat[A]] extends MatTrait[A, M] {

  implicit def scalar: MultiplicativeMonoid[A]

  import spire.syntax.multiplicativeMonoid._

  // builder methods

  def ones(rows: Int, cols: Int): M[A] =
    fill(rows, cols)(scalar.one)

  def times(lhs: A, rhs: Mat[A]): M[A] = pointwiseUnary(rhs)(lhs * _)

  def times(lhs: Mat[A], rhs: A): M[A] = pointwiseUnary(lhs)(_ * rhs)

  def pointwiseTimes(lhs: Mat[A], rhs: Mat[A]): M[A] = pointwiseBinary(lhs, rhs)(_ * _)

  def dyad(lhs: Vec[A], rhs: Vec[A]): M[A] = tabulate(lhs.length, rhs.length) { (r, c) => lhs(r) * rhs(c) }

}
