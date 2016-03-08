package scalin
package impl
package func

import spire.algebra.Field

trait MatField[A, MA <: Mat[A]]
    extends scalin.algebra.MatField[A, MA]
    with scalin.impl.func.MatEuclideanRing[A, MA] {

  implicit def scalar: Field[A]

  import spire.syntax.field._

  def pointwiseDiv(lhs: Mat[A], rhs: Mat[A]): MA = pointwiseBinary(lhs, rhs)(_ / _)

  def div(lhs: Mat[A], rhs: A): MA = pointwiseUnary(lhs)(_ / rhs)

  def luDecomposition(lhs: Mat[A]): LUDecomposition[A] = ???

  // inverse of matrix
  def inverse(lhs: Mat[A]): MA = ???

}
