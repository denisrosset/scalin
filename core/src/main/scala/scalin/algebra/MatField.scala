package scalin
package algebra

import spire.algebra.{Field, NRoot}

trait MatField[A, MA <: Mat[A]] extends MatEuclideanRing[A, MA] {

  implicit def scalar: Field[A]

  def pointwiseDiv(lhs: Mat[A], rhs: Mat[A]): MA

  def div(lhs: Mat[A], rhs: A): MA

  /** LU decomposition. */
  def luDecomposition(lhs: Mat[A]): LUDecomposition[A]

  /** Rank factorization, obtained by computing the reduced row echelon form. */
  def rankFactorization(lhs: Mat[A]): RankFactorization[A]

  /** Matrix inverse. Requires the matrix to be invertible, throws
    * an exception otherwise.
    */
  def inverse(lhs: Mat[A]): MA

  /** Returns the result of the Gram-Schmidt process applied on the matrix,
    * which is orthonormal.
    */
  def orthonormalized(lhs: Mat[A])(implicit ev: NRoot[A]): MA

}
