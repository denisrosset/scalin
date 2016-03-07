package scalin
package algebra

import spire.algebra._

import spire.syntax.cfor._

trait MatRing[A, MA <: Mat[A]] extends MatMultiplicativeMonoid[A, MA] {

  implicit def scalar: Ring[A]

  import spire.syntax.ring._

  // builder methods

  def zeros(rows: Int, cols: Int): MA =
    fill(rows, cols)(scalar.zero)

  def eye(n: Int): MA =
    tabulate(n, n)( (r, c) => if (r == c) scalar.one else scalar.zero )

  // additive group methods

  def plus(lhs: Mat[A], rhs: Mat[A]): MA = pointwiseBinary(lhs, rhs)(_ + _)

  def negate(lhs: Mat[A]): MA = pointwiseUnary(lhs)(-_)

  def minus(lhs: Mat[A], rhs: Mat[A]): MA = pointwiseBinary(lhs, rhs)(_ - _)

  def pointwisePlus(lhs: Mat[A], rhs: A): MA = pointwiseUnary(lhs)(_ + rhs)

  def pointwiseMinus(lhs: Mat[A], rhs: A): MA = pointwiseUnary(lhs)(_ - rhs)

  // ring methods

  def times(lhs: Mat[A], rhs: Mat[A]): MA = {
    import spire.syntax.cfor._
    val n = lhs.cols
    require(n == rhs.rows)
    if (n == 0)
      zeros(lhs.rows, rhs.cols)
    else
      tabulate(lhs.rows, rhs.cols) { (r, c) =>
        var sum = lhs(r, 0) * rhs(0, c)
        cforRange(1 until lhs.cols) { k =>
          sum += lhs(r, k) * rhs(k, c)
        }
        sum
      }
  }

  def frobenius(lhs: Mat[A], rhs: Mat[A]): A = {
    val nr = lhs.rows
    require(nr == rhs.rows)
    val nc = lhs.cols
    require(nc == rhs.cols)
    import spire.syntax.cfor._
    var sum = scalar.zero
    cforRange(0 until nr) { r =>
      cforRange(0 until nc) { c =>
        sum += lhs(r, c) * rhs(r, c)
      }
    }
    sum
  }

  def trace(lhs: Mat[A]): A = {
    val n = lhs.rows
    require(n == lhs.cols)
    if (n == 0) scalar.zero else {
      var sum = lhs(0, 0)
      cforRange(1 until n) { k =>
        sum += lhs(k, k)
      }
      sum
    }
  }

}
