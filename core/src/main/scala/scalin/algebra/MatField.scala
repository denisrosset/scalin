package scalin
package algebra

import spire.algebra.Field

import spire.syntax.field._

trait MatField[A, +MA <: Mat[A]] extends MatEuclideanRing[A, MA] {

  implicit def scalar: Field[A]

  def pointwiseDiv(lhs: Mat[A], rhs: Mat[A]): MA = pointwiseBinary(lhs, rhs)(_ / _)

  def div(lhs: Mat[A], rhs: A): MA = pointwiseUnary(lhs)(_ / rhs)

}
