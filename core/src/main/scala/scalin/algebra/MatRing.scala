package scalin
package algebra

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.ring._

trait MatRing[A, +MA <: Mat[A]] extends MatMultiplicativeMonoid[A, MA] {

  //// Creation

  /** Matrix filled with zeroes. */
  def zeros(rows: Int, cols: Int)(implicit A: AdditiveMonoid[A]): MA = fillConstant(rows, cols)(A.zero)

  /** Identity matrix. */
  def eye(n: Int)(implicit A: Rig[A]): MA = tabulate(n, n)( (r, c) => if (r == c) A.one else A.zero )

  def toDiagMat(lhs: Vec[A])(implicit A: AdditiveMonoid[A]): MA = tabulate(lhs.length, lhs.length)( (r, c) => if (r == c) lhs(r) else A.zero )

  //// Additive group methods

  def plus(lhs: Mat[A], rhs: Mat[A])(implicit A: AdditiveSemigroup[A]): MA = pointwiseBinary(lhs, rhs)(_ + _)

  def negate(lhs: Mat[A])(implicit A: AdditiveGroup[A]): MA = pointwiseUnary(lhs)(-_)

  def minus(lhs: Mat[A], rhs: Mat[A])(implicit A: AdditiveGroup[A]): MA = pointwiseBinary(lhs, rhs)(_ - _)

  def pointwisePlus(lhs: Mat[A], rhs: A)(implicit A: AdditiveSemigroup[A]): MA = pointwiseUnary(lhs)(_ + rhs)

  def pointwiseMinus(lhs: Mat[A], rhs: A)(implicit A: AdditiveGroup[A]): MA = pointwiseUnary(lhs)(_ - rhs)

  /** Returns the number of non-zero elements in the matrix. */
  def nnz(lhs: Mat[A])(implicit ev: Eq[A], A: AdditiveMonoid[A]): Int = count(lhs)(A.isZero(_))

  /** Computes the sum of all the matrix elements. */
  def sum(lhs: Mat[A])(implicit A: AdditiveMonoid[A]): A = fold(lhs)(A.zero)(A.plus)

  /** Trace of the matrix, equal to the sum of diagonal entries. Requires a square matrix. */
  def trace(lhs: Mat[A])(implicit A: AdditiveMonoid[A]): A = {
    val n = lhs.nRows
    require(n == lhs.nCols)
    if (n == 0) A.zero else {
      var sum = lhs(0, 0)
      cforRange(1 until n) { k =>
        sum += lhs(k, k)
      }
      sum
    }
  }

  //// Ring methods

  /** Matrix-matrix product. Requires `lhs.cols == rhs.rows`. */
  def times(lhs: Mat[A], rhs: Mat[A])(implicit A: Ring[A]): MA = {
    import spire.syntax.cfor._
    val n = lhs.nCols
    require(n == rhs.nRows)
    if (n == 0)
      zeros(lhs.nRows, rhs.nCols)
    else
      tabulate(lhs.nRows, rhs.nCols) { (r, c) =>
        var sum = lhs(r, 0) * rhs(0, c)
        cforRange(1 until lhs.nCols) { k =>
          sum += lhs(r, k) * rhs(k, c)
        }
        sum
      }
  }

  /** Frobenius product, sum of the Hadamard product elements.
    * 
    * See https://en.wikipedia.org/wiki/Frobenius_inner_product .*/
  def frobenius(lhs: Mat[A], rhs: Mat[A])(implicit A: Ring[A]): A = {
    val nr = lhs.nRows
    require(nr == rhs.nRows)
    val nc = lhs.nCols
    require(nc == rhs.nCols)
    import spire.syntax.cfor._
    var sum = A.zero
    cforRange(0 until nr) { r =>
      cforRange(0 until nc) { c =>
        sum += lhs(r, c) * rhs(r, c)
      }
    }
    sum
  }

}
