package scalin
package impl

import spire.algebra.Eq
import spire.syntax.eq._
import spire.syntax.cfor._

/** Non-optimized default implementation. */
trait VecEngine[A, VA <: Vec[A]] extends scalin.algebra.VecEngine[A, VA] {

  //// Helper methods

  protected def pointwiseUnary(lhs: Vec[A])(f: A => A) = tabulate(lhs.length)(k => f(lhs(k)))

  protected def pointwiseBinary(lhs: Vec[A], rhs: Vec[A])(f: (A, A) => A): VA = {
    require(lhs.length == rhs.length)
    tabulate(lhs.length)( k =>  f(lhs(k), rhs(k)) )
  }

  protected def booleanBinaryAnd(lhs: Vec[A], rhs: Vec[A])(f: (A, A) => Boolean): Boolean =
    (lhs.length == rhs.length) && {
      cforRange(0 until lhs.length) { k =>
        if (!f(lhs(k), rhs(k))) return false
      }
      true
    }

  protected def pointwiseBooleanUnary[B](lhs: Vec[B])(f: B => Boolean)(implicit ev: Boolean =:= A): VA = tabulate(lhs.length)( k => f(lhs(k)) )

  protected def pointwiseBooleanBinary[B](lhs: Vec[B], rhs: Vec[B])(f: (B, B) => Boolean)(implicit ev: Boolean =:= A): VA = {
    require(lhs.length == rhs.length)
    tabulate(lhs.length)( k => f(lhs(k), rhs(k)) )
  }

  //// Creation

  def empty: VA = tabulate(0)(sys.error("Cannot be called"))

  def fill(length: Int)(a: => A): VA = tabulate(length)( k => a )

  def fromSeq(elements: Seq[A]): VA = tabulate(elements.size)( elements(_) )

  def fromVec(vec: Vec[A]): VA = tabulate(vec.length)( k => vec(k) )

  def diag(mat: Mat[A]): VA = tabulate(spire.math.min(mat.nRows, mat.nCols))( k => mat(k, k) )

  //// Collection-like methods

  def count(lhs: Vec[A])(f: A => Boolean): Int = {
    var n = 0
    cforRange(0 until lhs.length) { k =>
      if (f(lhs(k)))
        n += 1
    }
    n
  }

  def fold[A1 >: A](lhs: Vec[A])(z: A1)(op: (A1, A1) => A1): A1 =
    if (lhs.length == 0) z
    else if (lhs.length == 1) lhs(0)
    else {
      var acc = op(lhs(0), lhs(1))
      cforRange(0 until lhs.length) { k =>
        acc = op(acc, lhs(k))
      }
      acc
    }

  def map[B](lhs: Vec[B])(f: B => A): VA = tabulate(lhs.length)( k => f(lhs(k)) )

  def colSeq(lhs: Mat[A]): IndexedSeq[VA] = IndexedSeq.tabulate(lhs.nCols)(c => colSlice(lhs, ::, c))

  def rowSeq(lhs: Mat[A]): IndexedSeq[VA] = IndexedSeq.tabulate(lhs.nRows)(r => rowSlice(lhs, r, ::))
  
  //// Slices

  def slice(vec: Vec[A], sub: Subscript): VA = {
    val ind = sub.forLength(vec.length)
    tabulate(ind.length)( k => vec(ind(k)) )
  }

  def slice(mat: Mat[A], sub: Subscript): VA = {
    val ind = sub.forLength(mat.nRows * mat.nCols)
    tabulate(ind.length) { k =>
      val ik = ind(k)
      val r = ik % mat.nRows
      val c = ik / mat.nRows
      mat(r, c)
    }
  }

  /** Slices a vector from a matrix, for the row `r` and column subscript `cs`. */
  def rowSlice(mat: Mat[A], r: Int, cs: Subscript): VA = {
    val ci = cs.forLength(mat.nCols)
    tabulate(ci.length)( k => mat(r, ci(k)) )
  }

  /** Slices a vector from a matrix, for the column `c` and the row subscript `rs`. */
  def colSlice(mat: Mat[A], rs: Subscript, c: Int): VA = {
    val ri = rs.forLength(mat.nRows)
    tabulate(ri.length)( k => mat(ri(k), c) )
  }

  //// With `Boolean =:= A`

  def pointwiseEqual[B](lhs: Vec[B], rhs: B)(implicit ev: Boolean =:= A): VA =
    pointwiseBooleanUnary(lhs)(_ == rhs)

  def pointwiseEqual[B](lhs: Vec[B], rhs: Vec[B])(implicit ev: Boolean =:= A): VA =
    pointwiseBooleanBinary(lhs, rhs)(_ == _)

  def pointwiseNotEqual[B](lhs: Vec[B], rhs: B)(implicit ev: Boolean =:= A): VA =
    pointwiseBooleanUnary(lhs)(_ != rhs)

  def pointwiseNotEqual[B](lhs: Vec[B], rhs: Vec[B])(implicit ev: Boolean =:= A): VA =
    pointwiseBooleanBinary(lhs, rhs)(_ != _)

  def pointwiseEqv[B](lhs: Vec[B], rhs: B)(implicit B: Eq[B], ev: Boolean =:= A): VA =
    pointwiseBooleanUnary(lhs)(_ === rhs)

  def pointwiseEqv[B](lhs: Vec[B], rhs: Vec[B])(implicit B: Eq[B], ev: Boolean =:= A): VA =
    pointwiseBooleanBinary(lhs, rhs)(_ === _)

  def pointwiseNeqv[B](lhs: Vec[B], rhs: B)(implicit B: Eq[B], ev: Boolean =:= A): VA =
    pointwiseBooleanUnary(lhs)(_ =!= rhs)

  def pointwiseNeqv[B](lhs: Vec[B], rhs: Vec[B])(implicit B: Eq[B], ev: Boolean =:= A): VA =
    pointwiseBooleanBinary(lhs, rhs)(_ =!= _)

  //// With `Eq[A]`

  def eqv(lhs: Vec[A], rhs: Vec[A])(implicit eqv: Eq[A]): Boolean = booleanBinaryAnd(lhs, rhs)(_ === _)

}
