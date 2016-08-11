package scalin.algebra

import spire.algebra._

/** Describes an involution interacting with a multiplicative monoid with the following laws:
  *
  * 1) (x * y).star = y.star * x.star
  * 2) x.star.star = x
  */
trait MultiplicativeInvolutiveSemigroup[A] extends MultiplicativeSemigroup[A] {

  def adjoint(x: A): A

  def isSelfAdjoint(x: A)(implicit equ: Eq[A]): A

}

/** Describes an involution interacting with a multiplicative monoid with the laws of `MultiplicativeInvolutiveSemigroup`
  * and the following addition:
  *
  * 1) id.star = id
 */
trait MultiplicativeInvolutiveMonoid[A] extends MultiplicativeInvolutiveSemigroup[A] with MultiplicativeMonoid[A] {


}

/** Describes an involution interacting with a multiplicative group with the laws of `MultiplicativeInvolutiveMonoid`
  * and the following addition:
  *
  * 1) a.reciprocal.adjoint = a.adjoint.reciprocal
  */
trait MultiplicativeInvolutiveGroup[A] extends MultiplicativeInvolutiveMonoid[A] with MultiplicativeGroup[A] {


}

trait MultiplicativeInvolutiveRing[A] extends MultiplicativeInvolutiveMonoid[A] with Ring[A] {

}

trait MultiplicativeInvolutiveField[A] extends MultiplicativeInvolutiveRing[A] with Field[A] {

}
