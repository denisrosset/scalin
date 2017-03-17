package scalin
package algos

import spire.algebra.Field

trait Inverse[A, MA <: scalin.Mat[A]] {

  def apply(mat: scalin.Mat[A]): MA

}

object Inverse {

  def apply[A, MA <: scalin.Mat[A]](implicit ev: Inverse[A, MA]): Inverse[A, MA] = ev

  implicit def denseInverse[A:Field:Pivot, MA <: scalin.Mat[A]](implicit MA: scalin.MatEngine[A, MA]): Inverse[A, MA] =
    new Inverse[A, MA] {
      import scalin.mutable.dense._
      def apply(mat: scalin.Mat[A]): MA = MA.fromMat(LUDecomposition.inverse(mat))
    }

}
