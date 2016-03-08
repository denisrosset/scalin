package scalin
package algebra

import spire.algebra._
import spire.syntax.cfor._

trait VecField[A, VA <: Vec[A]] extends VecEuclideanRing[A, VA] {

  type TC[A1, VA1 <: Vec[A1]] <: VecField[A1, VA1]

  implicit def scalar: Field[A]

  import spire.syntax.field._

  def pointwiseDiv(lhs: Vec[A], rhs: Vec[A]): VA = pointwiseBinary(lhs, rhs)(_ / _)

  def div(lhs: Vec[A], rhs: A): VA = pointwiseUnary(lhs)(_ / rhs)

}
