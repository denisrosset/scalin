package scalin

import spire.algebra._

/** A trait representing a concrete row vector, or a row vector node in 
  * an abstract syntax tree. */
abstract class AbstractRowVec[A] extends AbstractNode { lhs =>

  def apply(k: Int): A

  /** Returns the index of the next possibly non-zero element after index `k`. */
  def nextNonZero(k: Int = -1): Int

  def length: Int

  def get[V[A] <: Vec[A]](implicit f: AbstractVec[A] => V[A], w: Wrap[V]): w.R[A] = w.wrap(f(t))

  def +(rhs: AbstractRowVec[A])(implicit A: AdditiveSemigroup[A]): AbstractRowVec[A] = (lhs.t + rhs.t).t

  def -(rhs: AbstractRowVec[A])(implicit A: AdditiveGroup[A]): AbstractRowVec[A] = (lhs.t - rhs.t).t

  def *:(realLhs: A)(implicit A: MultiplicativeSemigroup[A]): AbstractRowVec[A] = (realLhs *: this.t).t

  def :*(rhs: A)(implicit A: MultiplicativeSemigroup[A]): AbstractRowVec[A] = (lhs.t :* rhs).t

  def :/(rhs: A)(implicit A: MultiplicativeGroup[A]): AbstractRowVec[A] = (lhs.t :/ rhs).t

  def unary_-(implicit A: AdditiveGroup[A]): AbstractRowVec[A] = (-lhs.t).t

  def t: AbstractVec[A] = ast.Vec.RowT(lhs)

  def dagger(implicit A: Involution[A]): AbstractVec[A] = ast.Vec.RowDagger(lhs)

  def *(rhs: AbstractVec[A])(implicit A: Rig[A]): A = {
    import spire.syntax.ring._
    val n = length
    require(n == rhs.length)
    var k = spire.math.min(
      nextNonZero(-1),
      rhs.nextNonZero(-1)
    )
    var res = A.zero
    while (k < n) {
      res += apply(k) * rhs(k)
      k = spire.math.min(
        nextNonZero(k),
        rhs.nextNonZero(k)
      )
    }
    res
  }

}
