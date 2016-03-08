package scalin
package impl

import spire.algebra.Eq
import spire.syntax.cfor._
import spire.syntax.eq._

trait MatEngine[A, MA <: Mat[A]] extends scalin.algebra.MatEngine[A, MA] {

  //// Helper methods

  def pointwiseUnary(lhs: Mat[A])(f: A => A) = tabulate(lhs.rows, lhs.cols)( (r, c) => f(lhs(r, c)) )

  def pointwiseBinary(lhs: Mat[A], rhs: Mat[A])(f: (A, A) => A): MA = {
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

  def pointwiseBooleanUnary[B](lhs: Mat[B])(f: B => Boolean)(implicit ev: Boolean =:= A): MA =
    tabulate(lhs.rows, lhs.cols)( (r, c) =>  f(lhs(r, c)) )

  def pointwiseBooleanBinary[B](lhs: Mat[B], rhs: Mat[B])(f: (B, B) => Boolean)(implicit ev: Boolean =:= A): MA = {
    require(lhs.rows == rhs.rows && lhs.cols == rhs.cols)
    tabulate(lhs.rows, lhs.cols)( (r, c) =>  f(lhs(r, c), rhs(r, c)) )
  }

  //// Creation

  def tabulate(rows: Int, cols: Int)(f: (Int, Int) => A): MA

  def fill(rows: Int, cols: Int)(a: => A): MA = tabulate(rows, cols)( (i, j) => a )

  def colMajor(rows: Int, cols: Int)(elements: A*): MA = {
    require(elements.size == rows * cols)
    tabulate(rows, cols)( (r, c) => elements(r + c * rows) )
  }

  def rowMajor(rows: Int, cols: Int)(elements: A*): MA = {
    require(elements.size == rows * cols)
    tabulate(rows, cols)( (r, c) => elements(c + r * cols) )
  }

  def rowMat(elements: A*): MA = rowMajor(1, elements.size)(elements: _*)

  def colMat(elements: A*): MA = colMajor(elements.size, 1)(elements: _*)

  def toRowMat(lhs: Vec[A]): MA = tabulate(1, lhs.length)( (r, c) => lhs(c) )

  def toColMat(lhs: Vec[A]): MA = tabulate(lhs.length, 1)( (r, c) => lhs(r) )

  //// Standard Java methods

  /** Tests if the two given matrices are equal. */
  def equal(lhs: Mat[A], rhs: Mat[A]): Boolean = booleanBinaryAnd(lhs, rhs)(_ == _)

  def hashCode(lhs: Mat[A]): Int = {
    import scala.util.hashing.MurmurHash3._
    val seed = 0x3CA7198A
    var a = 0
    var b = 1L
    var n = 0
    cforRange(0 until lhs.rows) { r =>
      cforRange(0 until lhs.cols) { c =>
        val hv = lhs(r, c).##
        if (hv != 0) {
          val hkv = (r * 41 + c) * 41 + hv
          a += hkv
          b *= (hkv | 1)
          n += 1
        }
      }
    }
    var h = seed
    h = mix(h, lhs.rows)
    h = mix(h, lhs.cols)
    h = mix(h, a)
    h = mix(h, b.toInt)
    h = mixLast(h, (b >> 32).toInt)
    finalizeHash(h, n)
  }

  //// Collection-like methods

  def count(lhs: Mat[A])(f: A => Boolean): Int = {
    var n = 0
    cforRange(0 until lhs.rows) { r =>
      cforRange(0 until lhs.cols) { c =>
        if (f(lhs(r, c)))
          n += 1
      }
    }
    n
  }

  def fold[A1 >: A](lhs: Mat[A])(z: A1)(op: (A1, A1) => A1): A1 =
    if (lhs.rows == 0 || lhs.cols == 0) z
    else if (lhs.rows == 1 && lhs.cols == 1) lhs(0, 0)
    else {
      var acc = z // could be optimized
      var i = 0
      // in column-major order
      cforRange(0 until lhs.cols) { c =>
        cforRange(0 until lhs.rows) { r =>
          acc = op(acc, lhs(r, c))
        }
      }
      acc
    }

  def map[B](lhs: Mat[B])(f: B => A): MA = tabulate(lhs.rows, lhs.cols)( (r, c) => f(lhs(r, c)) )

  //// Shuffling elements around

  def t(mat: Mat[A]): MA = tabulate(mat.cols, mat.rows)( (i, j) => mat(j, i) )

  def reshape(vec: Vec[A], rows1: Int, cols1: Int): MA = {
    require(vec.length == rows1 * cols1)
    tabulate(rows1, cols1)( (r1, c1) => vec(r1 + c1 * rows1) )
  }

  //// Slices

  def slice(mat: Mat[A], rs: Subscript, cs: Subscript): MA = {
    val ri = rs.forLength(mat.rows)
    val ci = cs.forLength(mat.cols)
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
