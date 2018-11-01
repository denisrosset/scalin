package scalin
package computation

class MatOps[A](val lhs: Mat[A]) extends AnyVal {
  /*
  /** LU decomposition. */
  def luDecomposition: LUDecomposition[A]

  /** Rank factorization, obtained by computing the reduced row echelon form. */
  def rankFactorization: RankFactorization[A]

  /** Matrix inverse. Requires the matrix to be invertible, throws
    * an exception otherwise.
    */
  def inverse[MA](implicit ev: Inverse[A, MA]): MA

  /** Returns the result of the Gram-Schmidt process applied on the matrix,
    * which is orthonormal.
    */
  def orthonormalized(implicit ev: Orthonormal[A]): MA

  /** Computes the rank of the matrix. */
  def rank(implicit ev: Rank[A]): Int

  /** Returns the result of the Gram-Schmidt process applied on the matrix,
    * which is orthogonal but not necessarily orthonormal.
    */
  def orthogonalized[MA](implicit ev: Orthogonalized[A, MA]): MA*/

}
