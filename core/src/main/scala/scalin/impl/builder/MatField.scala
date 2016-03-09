package scalin
package impl
package builder

trait MatField[A, MA <: Mat[A], UA <: mutable.Mat[A]]
    extends scalin.impl.MatField[A, MA]
    with scalin.impl.builder.MatEuclideanRing[A, MA, UA] {

  def luDecomposition(lhs: Mat[A]): LUDecomposition[A] = ??? // TODO

  // inverse of matrix
  def inverse(lhs: Mat[A]): MA = ??? // TODO

}
