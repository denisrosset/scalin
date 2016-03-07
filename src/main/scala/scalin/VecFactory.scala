package scalin

import spire.algebra.{AdditiveMonoid, MultiplicativeMonoid, Rig}
import spire.syntax.cfor._

trait VecFactory[V[A] <: Vec[A], Extra[_]] {

  def tabulate[A:Extra](length: Int)(f: Int => A): V[A]

}
