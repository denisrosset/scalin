package scalin
package ast

import spire.algebra._

object Mat {

  abstract class TransposeLike[A] extends AbstractMat[A] {
    def mat: AbstractMat[A]
    def nextNonZeroInCol(r: Int, c: Int) = mat.nextNonZeroInRow(c, r)
    def nextNonZeroInRow(r: Int, c: Int) = mat.nextNonZeroInCol(c, r)
    def rows: Int = mat.cols
    def cols: Int = mat.rows
    def touch(node: AbstractNode) = mat.touch(node) match {
      case Touch.Clean() => Touch.Clean()
      case Touch.Multi() => Touch.Multi()
      case Touch.Row(row) => Touch.Col(row)
      case Touch.Col(col) => Touch.Row(col)
      case Touch.AsIs() => Touch.Multi()
    }
  }

  case class Transpose[A](mat: AbstractMat[A]) extends TransposeLike[A] {
    def apply(r: Int, c: Int) = mat(r, c)
  }

  case class Star[A](mat: AbstractMat[A])(implicit A: Involution[A]) extends TransposeLike[A] {
    def apply(r: Int, c: Int) = A.dagger(mat(c, r))
  }

  abstract class Linear[A] extends AbstractMat[A] {

    def mat: AbstractMat[A]

    def nextNonZeroInCol(r: Int, c: Int) = mat.nextNonZeroInCol(r, c)
    def nextNonZeroInRow(r: Int, c: Int) = mat.nextNonZeroInRow(r, c)
    def rows: Int = mat.rows
    def cols: Int = mat.cols
    def touch(node: AbstractNode) = mat.touch(node)

  }

  case class Negate[A](mat: AbstractMat[A])(implicit A: AdditiveGroup[A]) extends Linear[A] {
    def apply(r: Int, c: Int): A = A.negate(mat(r, c))
  }

  case class LeftScalarTimes[A](lhs: A, mat: AbstractMat[A])(implicit A: MultiplicativeSemigroup[A]) extends Linear[A] {
    def apply(r: Int, c: Int): A = A.times(lhs, mat(r, c))
  }

  case class RightScalarTimes[A](mat: AbstractMat[A], rhs: A)(implicit A: MultiplicativeSemigroup[A]) extends Linear[A] {
    def apply(r: Int, c: Int): A = A.times(mat(r, c), rhs)
  }

  case class RightScalarDiv[A](mat: AbstractMat[A], rhs: A)(implicit A: MultiplicativeGroup[A]) extends Linear[A] {
    def apply(r: Int, c: Int): A = A.div(mat(r, c), rhs)
  }

}
