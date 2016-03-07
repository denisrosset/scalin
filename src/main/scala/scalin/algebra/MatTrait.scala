package scalin
package algebra

import spire.algebra._
import spire.syntax.order._
import spire.syntax.cfor._

trait MatTrait[A, M[A] <: Mat[A]] {

  // builders

  def tabulate(rows: Int, cols: Int)(f: (Int, Int) => A): M[A] // TODO = factory.tabulate[A](rows, cols)(f)

  def fill(rows: Int, cols: Int)(a: => A): M[A] = tabulate(rows, cols)( (i, j) => a )

  def colMajor(rows: Int, cols: Int)(elements: A*): M[A] = {
    require(elements.size == rows * cols)
    tabulate(rows, cols)( (r, c) => elements(r + c * rows) )
  }

  def rowMajor(rows: Int, cols: Int)(elements: A*): M[A] = {
    require(elements.size == rows * cols)
    tabulate(rows, cols)( (r, c) => elements(c + r * cols) )
  }

  def rowMat(elements: A*): M[A] = rowMajor(1, elements.size)(elements: _*)

  def colMat(elements: A*): M[A] = colMajor(elements.size, 1)(elements: _*)

  // shufflers

  def t(mat: Mat[A]): M[A] = tabulate(mat.cols, mat.rows)( (i, j) => mat(j, i) )

  def reshape(vec: Vec[A], rows1: Int, cols1: Int): M[A] = {
    require(vec.length == rows1 * cols1)
    tabulate(rows1, cols1)( (r1, c1) => vec(r1 + c1 * rows1) )
  }

  // slicers

  def slice(mat: Mat[A], rs: Subscript, cs: Subscript): M[A] = {
    val ri = rs.forLength(mat.rows)
    val ci = cs.forLength(mat.cols)
    tabulate(ri.length, ci.length)( (k, l) => mat(ri(k), ci(l)) )
  }

  // pointwise functions

  def pointwiseUnary(lhs: Mat[A])(f: A => A) = tabulate(lhs.rows, lhs.cols)( (r, c) => f(lhs(r, c)) )

  def pointwiseBinary(lhs: Mat[A], rhs: Mat[A])(f: (A, A) => A): M[A] = {
    require(lhs.rows == rhs.rows)
    require(lhs.cols == rhs.cols)
    tabulate(lhs.rows, lhs.cols)( (r, c) => f(lhs(r, c), rhs(r, c)) )
  }

  def booleanBinaryAnd(lhs: Mat[A], rhs: Mat[A])(f: (A, A) => Boolean): Boolean =
    (lhs.rows == rhs.rows && lhs.cols == rhs.cols) && {
      cforRange(0 until lhs.rows) { r =>
        cforRange(0 until lhs.cols) { c =>
          if (!f(lhs(r, c), rhs(r, c))) return false
        }
      }
      true
    }

  def pointwiseBooleanUnary[B](lhs: Mat[B])(f: B => Boolean)(implicit ev: Boolean =:= A): M[A] =
    tabulate(lhs.rows, lhs.cols)( (r, c) =>  f(lhs(r, c)) )

  def pointwiseBooleanBinary[B](lhs: Mat[B], rhs: Mat[B])(f: (B, B) => Boolean)(implicit ev: Boolean =:= A): M[A] = {
    require(lhs.rows == rhs.rows && lhs.cols == rhs.cols)
    tabulate(lhs.rows, lhs.cols)( (r, c) =>  f(lhs(r, c), rhs(r, c)) )
  }

  def pointwiseEqual[B](lhs: Mat[B], rhs: B)(implicit ev: Boolean =:= A): M[A] =
    pointwiseBooleanUnary(lhs)(_ == rhs)

  def pointwiseEqual[B](lhs: Mat[B], rhs: Mat[B])(implicit ev: Boolean =:= A): M[A] =
    pointwiseBooleanBinary(lhs, rhs)(_ == _)

  def pointwiseNotEqual[B](lhs: Mat[B], rhs: B)(implicit ev: Boolean =:= A): M[A] =
    pointwiseBooleanUnary(lhs)(_ != rhs)

  def pointwiseNotEqual[B](lhs: Mat[B], rhs: Mat[B])(implicit ev: Boolean =:= A): M[A] =
    pointwiseBooleanBinary(lhs, rhs)(_ != _)

  def pointwiseEqv[B](lhs: Mat[B], rhs: B)(implicit B: Eq[B], ev: Boolean =:= A): M[A] =
    pointwiseBooleanUnary(lhs)(_ === rhs)

  def pointwiseEqv[B](lhs: Mat[B], rhs: Mat[B])(implicit B: Eq[B], ev: Boolean =:= A): M[A] =
    pointwiseBooleanBinary(lhs, rhs)(_ === _)

  def pointwiseNeqv[B](lhs: Mat[B], rhs: B)(implicit B: Eq[B], ev: Boolean =:= A): M[A] =
    pointwiseBooleanUnary(lhs)(_ =!= rhs)

  def pointwiseNeqv[B](lhs: Mat[B], rhs: Mat[B])(implicit B: Eq[B], ev: Boolean =:= A): M[A] =
    pointwiseBooleanBinary(lhs, rhs)(_ =!= _)

  // equality functions

  def equal(lhs: Mat[A], rhs: Mat[A]): Boolean = booleanBinaryAnd(lhs, rhs)(_ == _)

  def eqv(lhs: Mat[A], rhs: Mat[A])(implicit eqv: Eq[A]): Boolean = booleanBinaryAnd(lhs, rhs)(_ === _)

  // def hashCode
}
