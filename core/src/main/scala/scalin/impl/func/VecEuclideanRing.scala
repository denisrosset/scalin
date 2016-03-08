package scalin
package impl
package func

import spire.algebra.EuclideanRing

trait VecEuclideanRing[A, VA <: Vec[A]]
    extends scalin.algebra.VecEuclideanRing[A, VA]
    with scalin.impl.func.VecRing[A, VA] {

  implicit def scalar: EuclideanRing[A]

  /** Computes the gcd of the elements of the vector. */
  def gcd(lhs: Vec[A]): A = fold(lhs)(scalar.zero)(scalar.gcd)

  /** Computes the lcm of the elements of the vector. */
  def lcm(lhs: Vec[A]): A = fold(lhs)(scalar.one)(scalar.lcm)

}
