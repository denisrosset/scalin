package scalin
package impl

import spire.algebra.Eq

trait VecEuclideanRing[A, VA <: Vec[A]]
    extends scalin.algebra.VecEuclideanRing[A, VA]
    with scalin.impl.VecRing[A, VA] {

  /** Computes the gcd of the elements of the vector. */
  def gcd(lhs: Vec[A])(implicit equ: Eq[A]): A = fold(lhs)(scalar.zero)(scalar.gcd)

  /** Computes the lcm of the elements of the vector. */
  def lcm(lhs: Vec[A])(implicit equ: Eq[A]): A = fold(lhs)(scalar.one)(scalar.lcm)

}
