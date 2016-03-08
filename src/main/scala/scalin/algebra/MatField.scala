package scalin
package algebra

import spire.algebra._
import spire.syntax.cfor._

trait MatField[A, MA <: Mat[A]] extends MatEuclideanRing[A, MA] {

  implicit def scalar: Field[A]

  import spire.syntax.field._

  def pointwiseDiv(lhs: Mat[A], rhs: Mat[A]): MA = pointwiseBinary(lhs, rhs)(_ / _)

  def div(lhs: Mat[A], rhs: A): MA = pointwiseUnary(lhs)(_ / rhs)

  def luDecomposition(lhs: Mat[A]): LUDecomposition[A] = ???

  // inverse of matrix
  def inverse(lhs: Mat[A]): MA = ???

}
