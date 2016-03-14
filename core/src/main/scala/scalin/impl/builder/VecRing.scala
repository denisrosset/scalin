package scalin
package impl
package builder

import spire.syntax.cfor._
import spire.syntax.ring._

import scalin.syntax.assign._

trait VecRing[A, VA <: Vec[A]]
    extends scalin.impl.VecRing[A, VA]
    with scalin.impl.builder.VecMultiplicativeMonoid[A, VA] {

  def times(lhs: Vec[A], rhs: Mat[A]): VA = {
    val n = lhs.length
    require(n == rhs.nRows)
    if (n == 0)
      zeros(rhs.nCols)
    else {
      val res = alloc(rhs.nCols)
      cforRange(0 until rhs.nCols) { c =>
        var sum = lhs(0) * rhs(0, c)
        cforRange(1 until n) { r =>
          sum += lhs(r) * rhs(r, c)
        }
        res(c) := sum
      }
      result(res)
    }
  }

  def times(lhs: Mat[A], rhs: Vec[A]): VA = {
    val n = rhs.length
    require(n == lhs.nCols)
    if (n == 0)
      zeros(lhs.nRows)
    else {
      val res = alloc(lhs.nRows)
      cforRange(0 until lhs.nRows) { r =>
        var sum = lhs(r, 0) * rhs(0)
        cforRange(1 until n) { c =>
          sum += lhs(r, c) * rhs(c)
        }
        res(r) := sum
      }
      result(res)
    }
  }

}
