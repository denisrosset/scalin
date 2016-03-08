package scalin
package algebra

import spire.algebra.Field

trait MatField[A, MA <: Mat[A]] extends MatEuclideanRing[A, MA] {

  implicit def scalar: Field[A]

  def pointwiseDiv(lhs: Mat[A], rhs: Mat[A]): MA

  def div(lhs: Mat[A], rhs: A): MA

  def luDecomposition(lhs: Mat[A]): LUDecomposition[A]

  /** Matrix inverse. Requires the matrix to be invertible, throws
    * an exception otherwise.
    */
  def inverse(lhs: Mat[A]): MA

}
