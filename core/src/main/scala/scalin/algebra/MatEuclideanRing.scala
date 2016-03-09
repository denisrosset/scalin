package scalin
package algebra

import spire.algebra.EuclideanRing

trait MatEuclideanRing[A, MA <: Mat[A]] extends MatRing[A, MA] {

  implicit def scalar: EuclideanRing[A]

  /** Computes the rank of the matrix. */
  def rank(lhs: Mat[A]): Int

  /** Computes the gcd of the elements of the matrix. */
  def gcd(lhs: Mat[A]): A

  /** Computes the lcm of the elements of the matrix. */
  def lcm(lhs: Mat[A]): A

  /** Returns the result of the Gram-Schmidt process applied on the matrix,
    * which is orthogonal but not necessarily orthonormal.
    */
  def orthogonalized(lhs: Mat[A]): MA

}
