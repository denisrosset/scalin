package scalin
package impl

import spire.syntax.field._

trait MatField[A, MA <: Mat[A]]
    extends scalin.algebra.MatField[A, MA]
    with scalin.impl.MatEuclideanRing[A, MA] {

  def pointwiseDiv(lhs: Mat[A], rhs: Mat[A]): MA = pointwiseBinary(lhs, rhs)(_ / _)

  def div(lhs: Mat[A], rhs: A): MA = pointwiseUnary(lhs)(_ / rhs)

}
