package scalin
package impl
package func

import spire.algebra._
import spire.syntax.order._
import spire.syntax.cfor._

trait VecFactory[A, VA <: Vec[A]] extends scalin.algebra.VecFactory[A, VA] {

  // creation

  def empty: VA = tabulate(0)(sys.error("Cannot be called"))

  def tabulate(length: Int)(f: Int => A): VA // = factory.tabulate[A](length)(f)

  def fill(length: Int)(a: => A): VA = tabulate(length)( k => a )

  def fromSeq(elements: Seq[A]): VA = tabulate(elements.size)( elements(_) )

  // shufflers

  // TODO: permute, ipermute

  // collection-like

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

  def cat(lhs: Vec[A], rhs: Vec[A]): VA = {
    val nl = lhs.length
    val nr = rhs.length
    tabulate(nl + nr)( k => if (k < nl) lhs(k) else rhs(k - nl) )
  }

  def flatMap[B](lhs: Vec[B])(f: B => Vec[A]): VA =
    if (lhs.length == 0) empty
    else if (lhs.length == 1) map[A](f(lhs(0)))(identity)
    else {
      var acc: VA = cat(f(lhs(0)), f(lhs(1)))
      cforRange(2 until lhs.length) { k =>
        acc = cat(acc, f(lhs(k)))
      }
      acc
    }

  // TODO: accelerate in subclasses, or find some other construction method like tabulate
  // that works well
  def flatten[B <: Vec[A]](lhs: Vec[B]): VA =
    if (lhs.length == 0) empty
    else if (lhs.length == 1) map[A](lhs(0))(identity)
    else {
      var acc = cat(lhs(0), lhs(1))
      cforRange(2 until lhs.length) { k =>
        acc = cat(acc, lhs(k))
      }
      acc
    }

  // slices

  def slice(vec: Vec[A], sub: Subscript): VA = {
    val ind = sub.forLength(vec.length)
    tabulate(ind.length)( k => vec(ind(k)) )
  }

  def slice(mat: Mat[A], sub: Subscript): VA = {
    val ind = sub.forLength(mat.rows * mat.cols)
    tabulate(ind.length) { k =>
      val ik = ind(k)
      val r = ik % mat.rows
      val c = ik / mat.rows
      mat(r, c)
    }
  }

  /** Slices a vector from a matrix, for the row `r` and column subscript `cs`. */
  def rowSlice(mat: Mat[A], r: Int, cs: Subscript): VA = {
    val ci = cs.forLength(mat.cols)
    tabulate(ci.length)( k => mat(r, ci(k)) )
  }

  /** Slices a vector from a matrix, for the column `c` and the row subscript `rs`. */
  def colSlice(mat: Mat[A], rs: Subscript, c: Int): VA = {
    val ri = rs.forLength(mat.rows)
    tabulate(ri.length)( k => mat(ri(k), c) )
  }

  def pointwiseUnary(lhs: Vec[A])(f: A => A) = tabulate(lhs.length)(k => f(lhs(k)))

  def pointwiseBinary(lhs: Vec[A], rhs: Vec[A])(f: (A, A) => A): VA = {
    require(lhs.length == rhs.length)
    tabulate(lhs.length)( k =>  f(lhs(k), rhs(k)) )
  }

  def booleanBinaryAnd(lhs: Vec[A], rhs: Vec[A])(f: (A, A) => Boolean): Boolean =
    (lhs.length == rhs.length) && {
      cforRange(0 until lhs.length) { k =>
        if (!f(lhs(k), rhs(k))) return false
      }
      true
    }

  def pointwiseBooleanUnary[B](lhs: Vec[B])(f: B => Boolean)(implicit ev: Boolean =:= A): VA = tabulate(lhs.length)( k => f(lhs(k)) )

  def pointwiseBooleanBinary[B](lhs: Vec[B], rhs: Vec[B])(f: (B, B) => Boolean)(implicit ev: Boolean =:= A): VA = {
    require(lhs.length == rhs.length)
    tabulate(lhs.length)( k => f(lhs(k), rhs(k)) )
  }

  // TODO: remove, and keep only the `equals` default Java method ?
  def equal(lhs: Vec[A], rhs: Vec[A]): Boolean = booleanBinaryAnd(lhs, rhs)(_ == _)

  def eqv(lhs: Vec[A], rhs: Vec[A])(implicit eqv: Eq[A]): Boolean = booleanBinaryAnd(lhs, rhs)(_ === _)

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

  def hashCode(lhs: Vec[A]): Int = ???

}
