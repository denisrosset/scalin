package scalin
package impl
package func

import spire.algebra._

trait MatEuclideanRing[A, MA <: Mat[A]]
    extends scalin.algebra.MatEuclideanRing[A, MA]
    with scalin.impl.func.MatRing[A, MA] {

  implicit def scalar: EuclideanRing[A]

  /** Computes the rank of the matrix. */
  def rank(lhs: Mat[A]): Int = ??? // TODO

  /** Computes the gcd of the elements of the matrix. */
  def gcd(lhs: Mat[A]): A = fold(lhs)(scalar.zero)(scalar.gcd)

  /** Computes the lcm of the elements of the matrix. */
  def lcm(lhs: Mat[A]): A = fold(lhs)(scalar.one)(scalar.lcm)

}
