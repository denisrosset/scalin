package scalin
package algebra

import spire.algebra.EuclideanRing

trait VecEuclideanRing[A, VA <: Vec[A]] extends VecRing[A, VA] {

  implicit def scalar: EuclideanRing[A]

  /** Computes the gcd of the elements of the vector. */
  def gcd(lhs: Vec[A]): A

  /** Computes the lcm of the elements of the vector. */
  def lcm(lhs: Vec[A]): A

}
