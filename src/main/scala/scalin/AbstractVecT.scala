package scalin

import spire.algebra._

/** A trait representing a concrete vector, or a vector node in an abstract syntax tree. */
case class AbstractVecT[A](t: AbstractVec[A]) extends AbstractNode { lhs =>

  def touch(node: AbstractNode) = lhs.t.touch(node)

  def +(rhs: AbstractRowVec[A])(implicit A: AdditiveSemigroup[A]): AbstractVecT[A] =
    AbstractVecT(new ast.VecVec.Plus(lhs.t, rhs.t))

  def -(rhs: AbstractRowVec[A])(implicit A: AdditiveGroup[A]): AbstractVecT[A] =
    AbstractVecT(new ast.VecVec.Minus(lhs.t, rhs.t))

  def *:(lhs: A)(implicit A: MultiplicativeSemigroup[A]): AbstractVecT[A] =
    AbstractVecT(new ast.Vec.LeftScalarTimes(lhs, this.t))

  def :*(rhs: A)(implicit A: MultiplicativeSemigroup[A]): AbstractVecT[A] =
    AbstractVecT(new ast.Vec.RightScalarTimes(lhs.t, rhs))

  def unary_-(implicit A: AdditiveGroup[A]): AbstractVecT[A] =
    AbstractVecT(new ast.Vec.Negate(lhs.t))

  def *(rhs: AbstractVec[A])(implicit A: Rig[A]): A = {
    import spire.syntax.ring._
    val n = lhs.t.length
    require(n == rhs.length)
    var k = spire.math.min(
      lhs.t.nextNonZero(-1),
      rhs.nextNonZero(-1)
    )
    var res = A.zero
    while (k < n) {
      res += lhs.t(k) * rhs(k)
      k = spire.math.min(
        lhs.t.nextNonZero(k),
        rhs.nextNonZero(k)
      )
    }
    res
  }

}
