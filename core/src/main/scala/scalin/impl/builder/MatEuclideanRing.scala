package scalin
package impl
package builder

trait MatEuclideanRing[A, MA <: Mat[A], UA <: mutable.Mat[A]]
    extends scalin.impl.MatEuclideanRing[A, MA]
    with scalin.impl.builder.MatRing[A, MA, UA] {

  implicit def UA: scalin.algebra.MatEuclideanRing[A, UA]

  /** Computes the rank of the matrix. */
  def rank(lhs: Mat[A]): Int = ??? // TODO

}
