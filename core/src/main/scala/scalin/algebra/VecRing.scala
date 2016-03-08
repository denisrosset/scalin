package scalin
package algebra

import spire.algebra._

trait VecRing[A, VA <: Vec[A]] extends VecMultiplicativeMonoid[A, VA] {

  type TC[A1, VA1 <: Vec[A1]] <: VecRing[A1, VA1]

  import spire.syntax.ring._
  import spire.syntax.cfor._

  implicit def scalar: Ring[A]

  // builder methods

  def zeros(length: Int): VA =
    fill(length)(scalar.zero)

  // additive group methods
  
  def plus(lhs: Vec[A], rhs: Vec[A]): VA = pointwiseBinary(lhs, rhs)(_ + _)

  def negate(lhs: Vec[A]): VA = pointwiseUnary(lhs)(-_)

  def minus(lhs: Vec[A], rhs: Vec[A]): VA = pointwiseBinary(lhs, rhs)(_ - _)

  def pointwisePlus(lhs: Vec[A], rhs: A): VA = pointwiseUnary(lhs)(_ + rhs)

  def pointwiseMinus(lhs: Vec[A], rhs: A): VA = pointwiseUnary(lhs)(_ - rhs)

  // ring methods

  // the matrix-vector, vector-matrix products belong to VecRing because of the Vec factory
  def times(lhs: Vec[A], rhs: Mat[A]): VA = {
    val n = lhs.length
    require(n == rhs.rows)
    if (n == 0)
      zeros(rhs.cols)
    else 
      tabulate(rhs.cols) { c =>
        var sum = lhs(0) * rhs(0, c)
        cforRange(1 until n) { r =>
          sum += lhs(r) * rhs(r, c)
        }
        sum
      }
  }


  def times(lhs: Mat[A], rhs: Vec[A]): VA = {
    val n = rhs.length
    require(n == lhs.cols)
    if (n == 0)
      zeros(lhs.rows)
    else
      tabulate(lhs.rows) { r =>
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

  def sum(lhs: Vec[A]): A = fold(lhs)(scalar.zero)(scalar.plus)

  def nnz(lhs: Vec[A])(implicit ev: Eq[A]): Int = count(lhs)(scalar.isZero(_))

}
