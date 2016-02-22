package scalin

import spire.algebra._

/** A trait representing a concrete matrix, or a matrix node in an abstract syntax tree. */
trait AbstractMat[A] extends AbstractNode { lhs =>

  def apply(r: Int, c: Int): A

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

  def :*(rhs: A)(implicit A: MultiplicativeSemigroup[A]): AbstractMat[A] =
    ast.Mat.Linear(lhs, a => A.times(a, rhs))

  def :/(rhs: A)(implicit A: MultiplicativeGroup[A]): AbstractMat[A] =
    ast.Mat.Linear(lhs, a => A.div(a, rhs))

  def *(rhs: AbstractMat[A])(implicit A: Semiring[A]): AbstractMat[A] =
    ast.MatMat.Times(lhs, rhs)

  def unary_-(implicit A: AdditiveGroup[A]): AbstractMat[A] =
    ast.Mat.Linear(lhs, A.negate)

  def t: AbstractMat[A] = ast.Mat.Transpose(lhs)

  def star(implicit A: Involution[A]): AbstractMat[A] = ast.Mat.Star(lhs)

}
