package scalin
package algebra

import spire.algebra.{Eq, Ring}

trait MatRing[A, +MA <: Mat[A]] extends MatMultiplicativeMonoid[A, MA] {

  implicit def scalar: Ring[A]

  //// Creation

  /** Matrix filled with zeroes. */
  def zeros(rows: Int, cols: Int): MA

  /** Identity matrix. */
  def eye(n: Int): MA

  def toDiagMat(lhs: Vec[A]): MA

  //// Additive group methods

  def plus(lhs: Mat[A], rhs: Mat[A]): MA

  def minus(lhs: Mat[A], rhs: Mat[A]): MA

  def negate(lhs: Mat[A]): MA

  def pointwisePlus(lhs: Mat[A], rhs: A): MA

  def pointwiseMinus(lhs: Mat[A], rhs: A): MA

  /** Returns the number of non-zero elements in the matrix. */
  def nnz(lhs: Mat[A])(implicit ev: Eq[A]): Int

  /** Computes the sum of all the matrix elements. */
  def sum(lhs: Mat[A]): A

  /** Trace of the matrix, equal to the sum of diagonal entries. Requires a square matrix. */
  def trace(lhs: Mat[A]): A

  //// Ring methods

  /** Matrix-matrix product. Requires `lhs.cols == rhs.rows`. */
  def times(lhs: Mat[A], rhs: Mat[A]): MA

  /** Frobenius product, sum of the Hadamard product elements.
    * 
    * See https://en.wikipedia.org/wiki/Matrix_multiplication#Frobenius_product .*/
  def frobenius(lhs: Mat[A], rhs: Mat[A]): A

  /** Computes the matrix determinant. Requires a square matrix. */
  def determinant(lhs: Mat[A]): A

}
