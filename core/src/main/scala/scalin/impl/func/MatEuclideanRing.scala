package scalin
package impl
package func

trait MatEuclideanRing[A, MA <: Mat[A]]
    extends scalin.impl.MatEuclideanRing[A, MA]
    with scalin.impl.func.MatRing[A, MA] {

  /** Computes the rank of the matrix. */
  def rank(lhs: Mat[A]): Int = ??? // TODO

}
