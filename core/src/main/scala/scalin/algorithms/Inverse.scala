package scalin
package algorithms

import scalin.decomposition.LU
import spire.algebra.Field

trait Inverse[A, MA <: scalin.Mat[A]] {

  def apply(mat: scalin.Mat[A]): MA

}

object Inverse {

  def apply[A, MA <: scalin.Mat[A]](implicit ev: Inverse[A, MA]): Inverse[A, MA] = ev

  implicit def denseInverse[A:Field:Pivot, MA <: scalin.Mat[A]](implicit MA: scalin.MatEngine[A, MA]): Inverse[A, MA] = {
    mat =>
      import scalin.immutable.dense._
      MA.fromMat(LU.inverse(mat.to[scalin.immutable.Mat[A]]))
    }

}
