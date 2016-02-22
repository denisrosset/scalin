package scalin
package ast

import spire.algebra._

object Mat {

  abstract class TransposeLike[A] extends AbstractMat[A] {
    def mat: AbstractMat[A]
    def nextNonZeroInCol(r: Int, c: Int) = mat.nextNonZeroInRow(c, r)
    def nextNonZeroInRow(r: Int, c: Int) = mat.nextNonZeroInCol(c, r)
    def rows = mat.cols
    def cols = mat.rows
  }

  case class Transpose[A](mat: AbstractMat[A]) extends TransposeLike[A] {
    def apply(r: Int, c: Int) = mat(r, c)
  }

  case class Star[A](mat: AbstractMat[A])(implicit A: Involution[A]) extends TransposeLike[A] {
    def apply(r: Int, c: Int) = A.dagger(mat(c, r))
  }

  case class Linear[A](mat: AbstractMat[A], f: A => A) extends AbstractMat[A] {

    def nextNonZeroInCol(r: Int, c: Int) = mat.nextNonZeroInCol(r, c)
    def nextNonZeroInRow(r: Int, c: Int) = mat.nextNonZeroInRow(r, c)
    def rows = mat.rows
    def cols = mat.cols
    def apply(r: Int, c: Int) = f(mat(r, c))
    
  }

  case class Elementwise[A, B](mat: AbstractMat[A], f: A => B) extends AbstractMat[B] {

    def nextNonZeroInCol(r: Int, c: Int) = r + 1
    def nextNonZeroInRow(r: Int, c: Int) = c + 1
    def rows = mat.rows
    def cols = mat.cols
    def apply(r: Int, c: Int): B = f(mat(r, c))

  }

}
