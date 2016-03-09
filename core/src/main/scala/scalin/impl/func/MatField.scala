package scalin
package impl
package func

trait MatField[A, MA <: Mat[A]]
    extends scalin.impl.MatField[A, MA]
    with scalin.impl.func.MatEuclideanRing[A, MA] {

  def luDecomposition(lhs: Mat[A]): LUDecomposition[A] = ??? // TODO

  // inverse of matrix
  def inverse(lhs: Mat[A]): MA = ??? // TODO

}
