package scalin
package algebra

import spire.algebra._
import spire.syntax.cfor._

trait VecEuclideanRing[A, VA <: Vec[A]] extends VecRing[A, VA] {

  type TC[A1, VA1 <: Vec[A1]] <: VecEuclideanRing[A1, VA1]

  implicit def scalar: EuclideanRing[A]

  import spire.syntax.euclideanRing._

  /** Computes the gcd of the elements of the vector. */
  def gcd(lhs: Vec[A]): A = fold(lhs)(scalar.zero)(scalar.gcd)

  /** Computes the lcm of the elements of the vector. */
  def lcm(lhs: Vec[A]): A = fold(lhs)(scalar.one)(scalar.lcm)

}
