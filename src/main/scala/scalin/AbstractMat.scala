package scalin

import spire.algebra._

/** A trait representing a concrete matrix, or a matrix node in an abstract syntax tree. */
trait AbstractMat[A] extends AbstractNode { lhs =>

  def apply(r: Int, c: Int): A

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
    ast.MatMat.Plus(lhs, rhs)

  def -(rhs: AbstractMat[A])(implicit A: AdditiveGroup[A]): AbstractMat[A] =
    ast.MatMat.Minus(lhs, rhs)

  def *:(realLhs: A)(implicit A: MultiplicativeSemigroup[A]): AbstractMat[A] =
    ast.Mat.LeftScalarTimes(realLhs, AbstractMat.this)

  def :*(rhs: A)(implicit A: MultiplicativeSemigroup[A]): AbstractMat[A] =
    ast.Mat.RightScalarTimes(lhs, rhs)

  def :/(rhs: A)(implicit A: MultiplicativeGroup[A]): AbstractMat[A] =
    ast.Mat.RightScalarDiv(lhs, rhs)

  def *(rhs: AbstractMat[A])(implicit A: Semiring[A]): AbstractMat[A] =
    ast.MatMat.Times(lhs, rhs)

  def unary_-(implicit A: AdditiveGroup[A]): AbstractMat[A] =
    ast.Mat.Negate(lhs)

  def t: AbstractMat[A] = ast.Mat.Transpose(lhs)

  def star(implicit A: Involution[A]): AbstractMat[A] = ast.Mat.Star(lhs)

}
