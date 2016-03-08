package scalin
package algebra

import spire.algebra.Field

trait VecField[A, VA <: Vec[A]] extends VecEuclideanRing[A, VA] {

  implicit def scalar: Field[A]

  def pointwiseDiv(lhs: Vec[A], rhs: Vec[A]): VA

  def div(lhs: Vec[A], rhs: A): VA

}
