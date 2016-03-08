package scalin
package algebra

import spire.algebra._

trait VecMultiplicativeMonoid[A, VA <: Vec[A]] extends VecFactory[A, VA] {

  implicit def scalar: MultiplicativeMonoid[A]

  //// Creation

  def ones(length: Int): VA

  //// With multiplicative monoid, returning scalar

  def product(lhs: Vec[A]): A

  //// With multiplicative monoid, returning vector

  def times(lhs: A, rhs: Vec[A]): VA

  def times(lhs: Vec[A], rhs: A): VA

  def pointwiseTimes(lhs: Vec[A], rhs: Vec[A]): VA

  def kron(lhs: Vec[A], rhs: Vec[A]): VA

}
