package scalin

import scalin.algebra._

/** LU Decomposition.
  * For an m-by-n matrix A with m >= n, the LU decomposition is an m-by-n
  * unit lower triangular matrix L, an n-by-n upper triangular matrix U,
  * and a permutation vector piv of length m so that A(piv,:) = L*U.
  * If m < n, then L is m-by-m and U is m-by-n.
  * 
  * The LU decompostion with pivoting always exists, even if the matrix is
  * singular, so the construction will never fail.  The primary use of the
  * LU decomposition is in the solution of square systems of simultaneous
  * linear equations. This will fail if isSingular returns true.
  */
trait LUDecomposition[A] { lhs =>

  type MA <: Mat[A]
  type VA <: Vec[A]

  def nPivots: Int
  def pivot(k: Int): Int

  def permutationCount: Int
  def permutation: MA

  def determinant: A

  def lower: MA
  def upper: MA
  def isSingular: Boolean

  def inverse: MA

  def solve(rhs: Mat[A]): MA
  def solve(rhs: Vec[A]): VA

}
