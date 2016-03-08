package scalin
package algebra

import spire.algebra._

trait VecMultiplicativeMonoid[A, VA <: Vec[A]] extends VecTrait[A, VA] {

  type TC[A1, VA1 <: Vec[A1]] <: VecMultiplicativeMonoid[A1, VA1]

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
