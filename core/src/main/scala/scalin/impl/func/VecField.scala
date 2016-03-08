package scalin
package impl
package func

import spire.algebra.Field

trait VecField[A, VA <: Vec[A]]
    extends scalin.algebra.VecField[A, VA]
    with scalin.impl.func.VecEuclideanRing[A, VA] {

  implicit def scalar: Field[A]

  import spire.syntax.field._

  def pointwiseDiv(lhs: Vec[A], rhs: Vec[A]): VA = pointwiseBinary(lhs, rhs)(_ / _)

  def div(lhs: Vec[A], rhs: A): VA = pointwiseUnary(lhs)(_ / rhs)

}
