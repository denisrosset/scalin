package scalin
package impl
package builder

import spire.syntax.cfor._
import spire.syntax.ring._

import scalin.syntax.assign._

trait VecRing[A, VA <: Vec[A], UA <: mutable.Vec[A]]
    extends scalin.impl.VecRing[A, VA]
    with scalin.impl.builder.VecMultiplicativeMonoid[A, VA, UA] {

  def times(lhs: Vec[A], rhs: Mat[A]): VA = {
    val n = lhs.length
    require(n == rhs.rows)
    if (n == 0)
      zeros(rhs.cols)
    else {
      val res = alloc(rhs.cols)
      cforRange(0 until rhs.cols) { c =>
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
    require(n == lhs.cols)
    if (n == 0)
      zeros(lhs.rows)
    else {
      val res = alloc(lhs.rows)
      cforRange(0 until lhs.rows) { r =>
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
