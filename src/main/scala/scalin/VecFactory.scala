package scalin

import spire.algebra.{AdditiveMonoid, MultiplicativeMonoid, Rig}
import spire.syntax.cfor._

trait VecFactory[V[A] <: Vec[A], Extra[_]] {

  def apply[A:Extra](elements: A*): V[A] = tabulate(elements.size)(elements(_))

  def tabulate[A:Extra](length: Int)(f: Int => A): V[A]

  def fill[A:Extra](length: Int)(a: => A): V[A] = tabulate(length)( k => a )

}
