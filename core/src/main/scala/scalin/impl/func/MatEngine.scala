package scalin
package impl
package func

import spire.syntax.cfor._

trait MatEngine[A, MA <: Mat[A]] extends scalin.impl.MatEngine[A, MA] {

  //// Creation

  def fillConstant(rows: Int, cols: Int)(a: A): MA = fill(rows, cols)(a)

  //// Collection-like methods

  // They have slow implementations due to the absence of a mutable builder.

  def horzcat(lhs: Mat[A], rhs: Mat[A]): MA = {
    val m = lhs.nRows
    require(m == rhs.nRows)
    val nl = lhs.nCols
    val nr = rhs.nCols
    tabulate(m, nl + nr)( (r, c) => if (c < nl) lhs(r, c) else rhs(r, c - nl) )
  }

  def vertcat(lhs: Mat[A], rhs: Mat[A]): MA = {
    val n = lhs.nCols
    require(n == rhs.nCols)
    val ml = lhs.nRows
    val mr = rhs.nRows
    tabulate(ml + mr, n)( (r, c) => if (r < ml) lhs(r, c) else rhs(r - ml, c) )
  }

  def flatten[B <: Mat[A]](lhs: Mat[B]): MA =
    if (lhs.nRows == 0 || lhs.nCols == 0) sys.error("Cannot flatten matrix with 0 rows or zero cols.")
    else {
      def flatRow(r: Int): MA = {
        if (lhs.nCols == 1) map(lhs(r, 0))(identity)
        else {
          var accRow = horzcat(lhs(r, 0), lhs(r, 1))
          cforRange(2 until lhs.nCols) { c =>
            accRow = horzcat(accRow, lhs(r, c))
          }
          accRow
        }
      }
      if (lhs.nRows == 1) flatRow(0)
      else {
        var acc = vertcat(flatRow(0), flatRow(1))
        cforRange(2 until lhs.nRows) { r =>
          acc = vertcat(acc, flatRow(r))
        }
        acc
      }
    }

  def flatMap[B](lhs: Mat[B])(f: B => Mat[A]): MA =
    if (lhs.nRows == 0 || lhs.nCols == 0) sys.error("Cannot flatten matrix with 0 rows or zero cols.")
    else {
      def flatRow(r: Int): MA = {
        if (lhs.nCols == 1) map(f(lhs(r, 0)))(identity)
        else {
          var accRow = horzcat(f(lhs(r, 0)), f(lhs(r, 1)))
          cforRange(2 until lhs.nCols) { c =>
            accRow = horzcat(accRow, f(lhs(r, c)))
          }
          accRow
        }
      }
      if (lhs.nRows == 1) flatRow(0)
      else {
        var acc = vertcat(flatRow(0), flatRow(1))
        cforRange(2 until lhs.nRows) { r =>
          acc = vertcat(acc, flatRow(r))
        }
        acc
      }
    }

}
