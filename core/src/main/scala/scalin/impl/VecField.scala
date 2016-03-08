package scalin
package impl

import spire.syntax.field._

trait VecField[A, VA <: Vec[A]]
    extends scalin.algebra.VecField[A, VA]
    with scalin.impl.VecEuclideanRing[A, VA] {

  def pointwiseDiv(lhs: Vec[A], rhs: Vec[A]): VA = pointwiseBinary(lhs, rhs)(_ / _)

  def div(lhs: Vec[A], rhs: A): VA = pointwiseUnary(lhs)(_ / rhs)

}
