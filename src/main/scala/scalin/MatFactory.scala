package scalin

import spire.algebra.{AdditiveMonoid, MultiplicativeMonoid, Rig}
import spire.syntax.cfor._

trait MatFactory[M[A] <: Mat[A], Extra[_]] {

  def tabulate[A:Extra](rows: Int, cols: Int)(f: (Int, Int) => A): M[A]

  def fill[A:Extra](rows: Int, cols: Int)(a: => A): M[A] = tabulate(rows, cols)( (r, c) => a )

  def zeros[A:AdditiveMonoid:Extra](rows: Int, cols: Int): M[A] =
    fill(rows, cols)(implicitly[AdditiveMonoid[A]].zero)

  def ones[A:Extra:MultiplicativeMonoid](rows: Int, cols: Int): M[A] =
    fill(rows, cols)(implicitly[MultiplicativeMonoid[A]].one)

  def eye[A:Extra:Rig](n: Int): M[A] =
    tabulate(n, n)( (r, c) => if (r == c) Rig[A].one else Rig[A].zero )

  implicit def fromAbstractMat[A:Extra](m: AbstractMat[A]): M[A] =
    tabulate(m.rows, m.cols)(m.apply)

}
