package scalin

import spire.algebra._
import spire.syntax.cfor._

/** A trait representing a concrete vector, or a vector node in an abstract syntax tree. */
trait AbstractVec[A] extends AbstractNode { lhs =>

  def apply(k: Int): A

  def apply(indices: Seq[Int]): scalin.SliceVec[A] = scalin.SliceVec(lhs, indices)

  def apply(mask: AbstractVec[Boolean]): scalin.SliceVec[A] = apply(Util.maskToIndices(mask))

  /** Returns the index of the next possibly non-zero element after index `k`. */
  def nextNonZero(k: Int = -1): Int

  def length: Int

  def get[V[A] <: Vec[A]](implicit f: AbstractVec[A] => V[A]): V[A] = f(lhs)

  def +(rhs: AbstractVec[A])(implicit A: AdditiveSemigroup[A]): AbstractVec[A] =
    ast.VecVec.Linear(lhs, rhs, A.plus)

  def -(rhs: AbstractVec[A])(implicit A: AdditiveGroup[A]): AbstractVec[A] =
    ast.VecVec.Linear(lhs, rhs, A.minus)

  def *:(realLhs: A)(implicit A: MultiplicativeSemigroup[A]): AbstractVec[A] =
    ast.Vec.Linear(lhs, a => A.times(realLhs, a))

  def :*(rhs: AbstractVec[A])(implicit A: MultiplicativeSemigroup[A]): AbstractVec[A] =
    ast.VecVec.ElementwiseProduct(lhs, rhs, A.times)

  def :*(rhs: A)(implicit A: MultiplicativeSemigroup[A]): AbstractVec[A] =
    ast.Vec.Linear(lhs, a => A.times(a, rhs))

  def :/(rhs: AbstractVec[A])(implicit A: MultiplicativeGroup[A]): AbstractVec[A] =
    ast.VecVec.Elementwise(lhs, rhs, A.div)

  def :/(rhs: A)(implicit A: MultiplicativeGroup[A]): AbstractVec[A] =
    ast.Vec.Linear(lhs, a => A.div(a, rhs))

  def :==(rhs: AbstractVec[A]): AbstractVec[Boolean] =
    ast.VecVec.Elementwise(lhs, rhs, (l: A, r: A) => l == r)

  def :==(rhs: A): AbstractVec[Boolean] =
    ast.Vec.Elementwise(lhs, (l: A) => (l == rhs))

  def :!=(rhs: AbstractVec[A]): AbstractVec[Boolean] =
    ast.VecVec.Elementwise(lhs, rhs, (l: A, r: A) => l != r)

  def :!=(rhs: A): AbstractVec[Boolean] =
    ast.Vec.Elementwise(lhs, (l: A) => (l != rhs))

  def :===(rhs: AbstractVec[A])(implicit A: Eq[A]): AbstractVec[Boolean] =
    ast.VecVec.Elementwise(lhs, rhs, A.eqv)

  def :===(rhs: A)(implicit A: Eq[A]): AbstractVec[Boolean] =
    ast.Vec.Elementwise(lhs, (l: A) => A.eqv(l, rhs))

  def :=!=(rhs: AbstractVec[A])(implicit A: Eq[A]): AbstractVec[Boolean] =
    ast.VecVec.Elementwise(lhs, rhs, A.neqv)

  def :=!=(rhs: A)(implicit A: Eq[A]): AbstractVec[Boolean] =
    ast.Vec.Elementwise(lhs, (l: A) => A.neqv(l, rhs))

  def :<(rhs: AbstractVec[A])(implicit A: PartialOrder[A]): AbstractVec[Boolean] =
    ast.VecVec.Elementwise(lhs, rhs, A.lt)

  def :<(rhs: A)(implicit A: PartialOrder[A]): AbstractVec[Boolean] =
    ast.Vec.Elementwise(lhs, (l: A) => A.lt(l, rhs))

  def :<=(rhs: AbstractVec[A])(implicit A: PartialOrder[A]): AbstractVec[Boolean] =
    ast.VecVec.Elementwise(lhs, rhs, A.lteqv)

  def :<=(rhs: A)(implicit A: PartialOrder[A]): AbstractVec[Boolean] =
    ast.Vec.Elementwise(lhs, (l: A) => A.lteqv(l, rhs))

  def :>(rhs: AbstractVec[A])(implicit A: PartialOrder[A]): AbstractVec[Boolean] =
    ast.VecVec.Elementwise(lhs, rhs, A.gt)

  def :>(rhs: A)(implicit A: PartialOrder[A]): AbstractVec[Boolean] =
    ast.Vec.Elementwise(lhs, (l: A) => A.gt(l, rhs))

  def :>=(rhs: AbstractVec[A])(implicit A: PartialOrder[A]): AbstractVec[Boolean] =
    ast.VecVec.Elementwise(lhs, rhs, A.gteqv)

  def :>=(rhs: A)(implicit A: PartialOrder[A]): AbstractVec[Boolean] =
    ast.Vec.Elementwise(lhs, (l: A) => A.gteqv(l, rhs))

  def unary_-(implicit A: AdditiveGroup[A]): AbstractVec[A] =
    ast.Vec.Linear(lhs, A.negate)

  def t: AbstractRowVec[A] = ast.Vec.ColT(lhs)

  def dagger(implicit A: Involution[A]): AbstractRowVec[A] = ast.Vec.ColDagger(lhs)

  def *(rhs: AbstractRowVec[A])(implicit A: MultiplicativeSemigroup[A]): AbstractMat[A] = ast.VecVec.Outer(lhs, rhs)

}
