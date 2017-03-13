package scalin
package impl

import spire.algebra.Eq

trait MatEuclideanRing[A, MA <: Mat[A]]
    extends scalin.algebra.MatEuclideanRing[A, MA]
    with scalin.impl.MatRing[A, MA] {

  /** Computes the gcd of the elements of the matrix. */
  def gcd(lhs: Mat[A])(implicit equ: Eq[A]): A = fold(lhs)(scalar.zero)(scalar.gcd)

  /** Computes the lcm of the elements of the matrix. */
  def lcm(lhs: Mat[A])(implicit equ: Eq[A]): A = fold(lhs)(scalar.one)(scalar.lcm)

}
