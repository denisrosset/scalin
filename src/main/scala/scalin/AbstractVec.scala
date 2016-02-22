package scalin

import spire.algebra._

/** A trait representing a concrete vector, or a vector node in an abstract syntax tree. */
trait AbstractVec[A] extends AbstractNode { lhs =>

  def apply(k: Int): A

  /** Returns the index of the next possibly non-zero element after index `k`. */
  def nextNonZero(k: Int = -1): Int

  def length: Int

  def get[V[A] <: Vec[A]](implicit f: AbstractVec[A] => V[A]): V[A] = f(lhs)

  def +(rhs: AbstractVec[A])(implicit A: AdditiveSemigroup[A]): AbstractVec[A] =
    ast.VecVec.Plus(lhs, rhs)

  def -(rhs: AbstractVec[A])(implicit A: AdditiveGroup[A]): AbstractVec[A] =
    ast.VecVec.Minus(lhs, rhs)

  def *:(realLhs: A)(implicit A: MultiplicativeSemigroup[A]): AbstractVec[A] =
    ast.Vec.LeftScalarTimes(realLhs, AbstractVec.this)

  def :*(rhs: A)(implicit A: MultiplicativeSemigroup[A]): AbstractVec[A] =
    ast.Vec.RightScalarTimes(lhs, rhs)

  def :/(rhs: A)(implicit A: MultiplicativeGroup[A]): AbstractVec[A] =
    ast.Vec.RightScalarDiv(lhs, rhs)

  def unary_-(implicit A: AdditiveGroup[A]): AbstractVec[A] =
    ast.Vec.Negate(lhs)

  def t: AbstractRowVec[A] = ast.Vec.ColT(lhs)

  def dagger(implicit A: Involution[A]): AbstractRowVec[A] = ast.Vec.ColDagger(lhs)

  def *(rhs: AbstractRowVec[A])(implicit A: MultiplicativeSemigroup[A]): AbstractMat[A] = ast.VecVec.Outer(lhs, rhs)

}
