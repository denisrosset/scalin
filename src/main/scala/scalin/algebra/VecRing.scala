package scalin
package algebra

import spire.algebra._

trait VecRing[A, V[A] <: Vec[A]] extends VecMultiplicativeMonoid[A, V] {
  import spire.syntax.ring._
  import spire.syntax.cfor._

  implicit def scalar: Ring[A]

  // builder methods

  def zeros(length: Int): V[A] =
    factory.fill(length)(scalar.zero)

  // additive group methods
  
  def plus(lhs: Vec[A], rhs: Vec[A]): V[A] = pointwiseBinary(lhs, rhs)(_ + _)

  def negate(lhs: Vec[A]): V[A] = pointwiseUnary(lhs)(-_)

  def minus(lhs: Vec[A], rhs: Vec[A]): V[A] = pointwiseBinary(lhs, rhs)(_ - _)

  def pointwisePlus(lhs: Vec[A], rhs: A): V[A] = pointwiseUnary(lhs)(_ + rhs)

  def pointwiseMinus(lhs: Vec[A], rhs: A): V[A] = pointwiseUnary(lhs)(_ - rhs)

  // ring methods

  // the matrix-vector, vector-matrix products belong to VecRing because of the Vec factory
  def times(lhs: Vec[A], rhs: Mat[A]): V[A] = {
    val n = lhs.length
    require(n == rhs.rows)
    if (n == 0)
      zeros(rhs.cols)
    else 
      factory.tabulate(rhs.cols) { c =>
        var sum = lhs(0) * rhs(0, c)
        cforRange(1 until n) { r =>
          sum += lhs(r) * rhs(r, c)
        }
        sum
      }
  }


  def times(lhs: Mat[A], rhs: Vec[A]): V[A] = {
    val n = rhs.length
    require(n == lhs.cols)
    if (n == 0)
      zeros(lhs.rows)
    else
      factory.tabulate(lhs.rows) { r =>
        var sum = lhs(r, 0) * rhs(0)
        cforRange(1 until n) { c =>
          sum += lhs(r, c) * rhs(c)
        }
        sum
      }
  }

  // dot product

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

}
