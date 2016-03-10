package scalin
package impl
package func

import spire.syntax.cfor._
import spire.syntax.ring._

trait VecRing[A, VA <: Vec[A]]
    extends scalin.impl.VecRing[A, VA]
    with scalin.impl.func.VecMultiplicativeMonoid[A, VA] {

  def times(lhs: Vec[A], rhs: Mat[A]): VA = {
    val n = lhs.length
    require(n == rhs.rows)
    if (n == 0)
      zeros(rhs.cols)
    else 
      tabulate(rhs.cols) { c =>
        var sum = lhs(0) * rhs(0, c)
        cforRange(1 until n) { r =>
          sum += lhs(r) * rhs(r, c)
        }
        sum
      }
  }

  def times(lhs: Mat[A], rhs: Vec[A]): VA = {
    val n = rhs.length
    require(n == lhs.cols)
    if (n == 0)
      zeros(lhs.rows)
    else
      tabulate(lhs.rows) { r =>
        var sum = lhs(r, 0) * rhs(0)
        cforRange(1 until n) { c =>
          sum += lhs(r, c) * rhs(c)
        }
        sum
      }
  }

}
