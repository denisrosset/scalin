package scalin
package impl
package func

import spire.syntax.cfor._

trait MatEngine[A, MA <: Mat[A]] extends scalin.impl.MatEngine[A, MA]

trait MatEuclideanRing[A, MA <: Mat[A]]
    extends scalin.impl.MatEuclideanRing[A, MA]
    with scalin.impl.func.MatRing[A, MA] {

  /** Computes the rank of the matrix. */
  def rank(lhs: Mat[A]): Int = ??? // TODO

}


trait MatField[A, MA <: Mat[A]]
    extends scalin.impl.MatField[A, MA]
    with scalin.impl.func.MatEuclideanRing[A, MA] {

  def luDecomposition(lhs: Mat[A]): LUDecomposition[A] = ??? // TODO

  // inverse of matrix
  def inverse(lhs: Mat[A]): MA = ??? // TODO

}

trait MatMultiplicativeMonoid[A, MA <: Mat[A]]
    extends scalin.impl.MatMultiplicativeMonoid[A, MA]
    with scalin.impl.func.MatEngine[A, MA]

trait MatRing[A, MA <: Mat[A]]
    extends scalin.impl.MatRing[A, MA]
    with scalin.impl.func.MatMultiplicativeMonoid[A, MA] {

  def determinant(lhs: Mat[A]): A = ??? // TODO: call mutable implementation

}
