package scalin.decomposition

import scalin.immutable.{Mat, Vec}
import spire.algebra._

object RealEigen {

  /** Decomposes A = V * D * V.t */
  def apply[A:Epsilon:Field:Signed:NRoot](A: scalin.immutable.Mat[A]): RealEigen[A] = {
    require(A.isSquare)
    if (A.isSymmetric) new SymRealEigen(A) else new NonSymRealEigen(A)
  }

}

/** Eigenvalue decomposition over the reals.
  *
  * Converted from
  * https://math.nist.gov/javanumerics/jama/
  *
  * If A is symmetric, then A = V*D*V' where the eigenvalue matrix D is
  * diagonal and the eigenvector matrix V is orthogonal.
  *     I.e. A = V * D * V.t and V * V.t is the identity matrix.
  *
  * If A is not symmetric, then the eigenvalue matrix D is block diagonal
  * with the real eigenvalues in 1-by-1 blocks and any complex eigenvalues,
  * lambda + i*mu, in 2-by-2 blocks, [lambda, mu; -mu, lambda].  The
  * columns of V represent the eigenvectors in the sense that A*V = V*D,
  * The matrix V may be badly
  * conditioned, or even singular, so the validity of the equation
  * A = V*D*inverse(V) depends upon V.cond().
  */
trait RealEigen[A] {

  /** Orthonormal change of basis matrix. */
  def V: Mat[A]
  /** Diagonal (for symmetric matrices), or diagonal by 2x2 blocks (for nonsymmetric matrices) */
  def D: Mat[A]

  def evRealPart: Vec[A]
  def evImagPart: Vec[A]

  def value(implicit A: Field[A], ev: scalin.immutable.MatEngine[A]): Mat[A] = V * D * V.t
}
