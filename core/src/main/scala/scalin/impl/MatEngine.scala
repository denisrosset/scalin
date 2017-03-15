package scalin
package impl

import spire.algebra.Eq
import spire.syntax.cfor._
import spire.syntax.eq._

trait MatEngine[A, MA <: Mat[A]] extends scalin.algebra.MatEngine[A, MA] {

  //// Creation

  def tabulate(rows: Int, cols: Int)(f: (Int, Int) => A): MA




  //// Collection-like methods

  def count(lhs: Mat[A])(f: A => Boolean): Int = {
    var n = 0
    cforRange(0 until lhs.nRows) { r =>
      cforRange(0 until lhs.nCols) { c =>
        if (f(lhs(r, c)))
          n += 1
      }
    }
    n
  }

  def fold[A1 >: A](lhs: Mat[A])(z: A1)(op: (A1, A1) => A1): A1 =
    if (lhs.nRows == 0 || lhs.nCols == 0) z
    else if (lhs.nRows == 1 && lhs.nCols == 1) lhs(0, 0)
    else {
      var acc = z // could be optimized
      var i = 0
      // in column-major order
      cforRange(0 until lhs.nCols) { c =>
        cforRange(0 until lhs.nRows) { r =>
          acc = op(acc, lhs(r, c))
        }
      }
      acc
    }

  def map[B](lhs: Mat[B])(f: B => A): MA = tabulate(lhs.nRows, lhs.nCols)((r, c) => f(lhs(r, c)) )

  //// Shuffling elements around

  def t(mat: Mat[A]): MA = tabulate(mat.nCols, mat.nRows)((i, j) => mat(j, i) )

  def reshape(vec: Vec[A], rows1: Int, cols1: Int): MA = {
    require(vec.length == rows1 * cols1)
    tabulate(rows1, cols1)( (r1, c1) => vec(r1 + c1 * rows1) )
  }

  //// Slices

  def slice(mat: Mat[A], rs: Subscript, cs: Subscript): MA = {
    val ri = rs.forLength(mat.nRows)
    val ci = cs.forLength(mat.nCols)
    tabulate(ri.length, ci.length)( (k, l) => mat(ri(k), ci(l)) )
  }

  //// With `Boolean =:= A`

  def pointwiseEqual[B](lhs: Mat[B], rhs: B)(implicit ev: Boolean =:= A): MA =
    pointwiseBooleanUnary(lhs)(_ == rhs)

  def pointwiseEqual[B](lhs: Mat[B], rhs: Mat[B])(implicit ev: Boolean =:= A): MA =
    pointwiseBooleanBinary(lhs, rhs)(_ == _)

  def pointwiseNotEqual[B](lhs: Mat[B], rhs: B)(implicit ev: Boolean =:= A): MA =
    pointwiseBooleanUnary(lhs)(_ != rhs)

  def pointwiseNotEqual[B](lhs: Mat[B], rhs: Mat[B])(implicit ev: Boolean =:= A): MA =
    pointwiseBooleanBinary(lhs, rhs)(_ != _)

  def pointwiseEqv[B](lhs: Mat[B], rhs: B)(implicit B: Eq[B], ev: Boolean =:= A): MA =
    pointwiseBooleanUnary(lhs)(_ === rhs)

  def pointwiseEqv[B](lhs: Mat[B], rhs: Mat[B])(implicit B: Eq[B], ev: Boolean =:= A): MA =
    pointwiseBooleanBinary(lhs, rhs)(_ === _)

  def pointwiseNeqv[B](lhs: Mat[B], rhs: B)(implicit B: Eq[B], ev: Boolean =:= A): MA =
    pointwiseBooleanUnary(lhs)(_ =!= rhs)

  def pointwiseNeqv[B](lhs: Mat[B], rhs: Mat[B])(implicit B: Eq[B], ev: Boolean =:= A): MA =
    pointwiseBooleanBinary(lhs, rhs)(_ =!= _)

  //// With `Eq[A]`

  def eqv(lhs: Mat[A], rhs: Mat[A])(implicit eqv: Eq[A]): Boolean = booleanBinaryAnd(lhs, rhs)(_ === _)

}
