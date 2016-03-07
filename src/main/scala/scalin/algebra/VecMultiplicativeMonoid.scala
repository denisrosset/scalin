package scalin
package algebra

import spire.algebra._

trait VecMultiplicativeMonoid[A, V[A] <: Vec[A]] extends VecTrait[A, V] {

  implicit def scalar: MultiplicativeMonoid[A]

  import spire.syntax.multiplicativeMonoid._

  // builder methods

  def ones(length: Int): V[A] =
    fill(length)(scalar.one)

  def times(lhs: A, rhs: Vec[A]): V[A] = pointwiseUnary(rhs)(lhs * _)

  def times(lhs: Vec[A], rhs: A): V[A] = pointwiseUnary(lhs)(_ * rhs)

  def pointwiseTimes(lhs: Vec[A], rhs: Vec[A]): V[A] = pointwiseBinary(lhs, rhs)(_ * _)

}
