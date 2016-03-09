package scalin
package impl

trait MatEuclideanRing[A, MA <: Mat[A]]
    extends scalin.algebra.MatEuclideanRing[A, MA]
    with scalin.impl.MatRing[A, MA] {

  /** Computes the gcd of the elements of the matrix. */
  def gcd(lhs: Mat[A]): A = fold(lhs)(scalar.zero)(scalar.gcd)

  /** Computes the lcm of the elements of the matrix. */
  def lcm(lhs: Mat[A]): A = fold(lhs)(scalar.one)(scalar.lcm)

}
