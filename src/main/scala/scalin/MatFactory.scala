package scalin

import spire.algebra.{AdditiveMonoid, MultiplicativeMonoid, Rig}
import spire.syntax.cfor._

trait MatFactory[M[A] <: Mat[A], Extra[_]] {

  def tabulate[A:Extra](rows: Int, cols: Int)(f: (Int, Int) => A): M[A]

  def fill[A:Extra](rows: Int, cols: Int)(a: => A): M[A] = tabulate(rows, cols)( (r, c) => a )

  def verbatim[A:Extra](rows: Int, cols: Int)(elements: A*): M[A] = tabulate(rows, cols)( (r, c) => elements(r * cols + c) )

}
