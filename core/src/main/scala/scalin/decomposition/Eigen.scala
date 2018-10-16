package scalin.decomposition

import scalin.immutable.{Mat, Vec}
import spire.algebra._

object Eigen {

  def apply[A:Epsilon:Field:Signed:NRoot](A: scalin.immutable.Mat[A]): Eigen[A] = {
    require(A.isSquare)
    if (A.isSymmetric) new SymEigen(A) else new NonSymEigen(A)
  }

}

trait Eigen[A] {

  def V: Mat[A]
  def D: Mat[A]

  def evRealPart: Vec[A]
  def evImagPart: Vec[A]

}
