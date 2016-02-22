package scalin

import spire.algebra._

/** A trait representing a concrete matrix, or a matrix node in an abstract syntax tree. */
trait AbstractMat[A] extends AbstractNode { lhs =>

  def apply(r: Int, c: Int): A

  def intersects(
//  def apply(rs: Seq[Int], cs: Seq[Int]): MatSlice[A, lhs.type] = MatSlice(lhs, rs, cs)

  /** Returns the index of the next possibly non-zero element in the row `r`
    * after column index `c`. */
  def nextNonZeroInRow(r: Int, c: Int = -1): Int

  /** Returns the index of the next possibly non-zero element in the column `c`
    * after row index `r`. */
  def nextNonZeroInCol(r: Int = -1, c: Int): Int

  def rows: Int

  def cols: Int

  def get[M[A] <: Mat[A]](implicit f: AbstractMat[A] => M[A]): M[A] = f(lhs)

  def +(rhs: AbstractMat[A])(implicit A: AdditiveSemigroup[A]): AbstractMat[A] =
    ast.MatMat.Linear(lhs, rhs, A.plus)

  def -(rhs: AbstractMat[A])(implicit A: AdditiveGroup[A]): AbstractMat[A] =
    ast.MatMat.Linear(lhs, rhs, A.minus)

  def *:(realLhs: A)(implicit A: MultiplicativeSemigroup[A]): AbstractMat[A] =
    ast.Mat.Linear(lhs, a => A.times(realLhs, a))

  def :*(rhs: AbstractMat[A])(implicit A: MultiplicativeSemigroup[A]): AbstractMat[A] =
    ast.MatMat.ElementwiseProduct(lhs, rhs, A.times)

  def :*(rhs: A)(implicit A: MultiplicativeSemigroup[A]): AbstractMat[A] =
    ast.Mat.Linear(lhs, a => A.times(a, rhs))

  def :/(rhs: AbstractMat[A])(implicit A: MultiplicativeGroup[A]): AbstractMat[A] =
    ast.MatMat.Elementwise(lhs, rhs, A.div)

  def :/(rhs: A)(implicit A: MultiplicativeGroup[A]): AbstractMat[A] =
    ast.Mat.Linear(lhs, a => A.div(a, rhs))

  def *(rhs: AbstractMat[A])(implicit A: Semiring[A]): AbstractMat[A] =
    ast.MatMat.Times(lhs, rhs)

  def :==(rhs: AbstractMat[A]): AbstractMat[Boolean] =
    ast.MatMat.Elementwise(lhs, rhs, (l: A, r: A) => l == r)

  def :==(rhs: A): AbstractMat[Boolean] =
    ast.Mat.Elementwise(lhs, (l: A) => (l == rhs))

  def :!=(rhs: AbstractMat[A]): AbstractMat[Boolean] =
    ast.MatMat.Elementwise(lhs, rhs, (l: A, r: A) => l != r)

  def :!=(rhs: A): AbstractMat[Boolean] =
    ast.Mat.Elementwise(lhs, (l: A) => (l != rhs))

  def :===(rhs: AbstractMat[A])(implicit A: Eq[A]): AbstractMat[Boolean] =
    ast.MatMat.Elementwise(lhs, rhs, A.eqv)

  def :===(rhs: A)(implicit A: Eq[A]): AbstractMat[Boolean] =
    ast.Mat.Elementwise(lhs, (l: A) => A.eqv(l, rhs))

  def :=!=(rhs: AbstractMat[A])(implicit A: Eq[A]): AbstractMat[Boolean] =
    ast.MatMat.Elementwise(lhs, rhs, A.neqv)

  def :=!=(rhs: A)(implicit A: Eq[A]): AbstractMat[Boolean] =
    ast.Mat.Elementwise(lhs, (l: A) => A.neqv(l, rhs))

  def :<(rhs: AbstractMat[A])(implicit A: PartialOrder[A]): AbstractMat[Boolean] =
    ast.MatMat.Elementwise(lhs, rhs, A.lt)

  def :<(rhs: A)(implicit A: PartialOrder[A]): AbstractMat[Boolean] =
    ast.Mat.Elementwise(lhs, (l: A) => A.lt(l, rhs))

  def :<=(rhs: AbstractMat[A])(implicit A: PartialOrder[A]): AbstractMat[Boolean] =
    ast.MatMat.Elementwise(lhs, rhs, A.lteqv)

  def :<=(rhs: A)(implicit A: PartialOrder[A]): AbstractMat[Boolean] =
    ast.Mat.Elementwise(lhs, (l: A) => A.lteqv(l, rhs))

  def :>(rhs: AbstractMat[A])(implicit A: PartialOrder[A]): AbstractMat[Boolean] =
    ast.MatMat.Elementwise(lhs, rhs, A.gt)

  def :>(rhs: A)(implicit A: PartialOrder[A]): AbstractMat[Boolean] =
    ast.Mat.Elementwise(lhs, (l: A) => A.gt(l, rhs))

  def :>=(rhs: AbstractMat[A])(implicit A: PartialOrder[A]): AbstractMat[Boolean] =
    ast.MatMat.Elementwise(lhs, rhs, A.gteqv)

  def :>=(rhs: A)(implicit A: PartialOrder[A]): AbstractMat[Boolean] =
    ast.Mat.Elementwise(lhs, (l: A) => A.gteqv(l, rhs))

  def unary_-(implicit A: AdditiveGroup[A]): AbstractMat[A] =
    ast.Mat.Linear(lhs, A.negate)

  def t: AbstractMat[A] = ast.Mat.Transpose(lhs)

  def star(implicit A: Involution[A]): AbstractMat[A] = ast.Mat.Star(lhs)

}
