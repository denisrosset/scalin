package scalin

import scalin.algebra._

trait MatType[M[A] <: Mat[A]] {

  type TC[_]

  def engine[A:TC]: MatEngine[A, M[A]]

}
