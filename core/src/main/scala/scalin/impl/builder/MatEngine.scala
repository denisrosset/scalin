package scalin
package impl
package builder

import scalin.syntax.assign._

import spire.syntax.cfor._

trait MatEngine[A, MA <: Mat[A], UA <: mutable.Mat[A]] extends scalin.impl.MatEngine[A, MA] {

  //// Mutable variant

  implicit def UA: scalin.algebra.MatEngine[A, UA]

  /** Returns a mutable matrix of the given shape. The initial content of the matrix is undefined. */
  def alloc(rows: Int, cols: Int): UA

  /** Converts the mutable matrix `mutable` into the desired instance. `mutable` is destroyed in
    * the process. */
  def result(mutable: UA): MA

  //// Creation

  def fillConstant(rows: Int, cols: Int)(a: A): MA = {
    val res = alloc(rows, cols)
    // TODO: optimize by copying columns after slice assignment is optimized
    // to not perform temporary copies
    cforRange(0 until cols) { c =>
      cforRange(0 until rows) { r =>
        res(r, c) := a
      }
    }
    result(res)
  }

  //// Collection-like methods

  // They have slow implementations due to the absence of a mutable builder.

  def horzcat(lhs: Mat[A], rhs: Mat[A]): MA = {
    val m = lhs.rows
    require(m == rhs.rows)
    val nl = lhs.cols
    val nr = rhs.cols
    val res = alloc(m, nl + nr)
    res(::, 0 until nl) := lhs
    res(::, nl until nl + nr) := rhs
    result(res)
  }

  def vertcat(lhs: Mat[A], rhs: Mat[A]): MA = {
    val n = lhs.cols
    require(n == rhs.cols)
    val ml = lhs.rows
    val mr = rhs.rows
    val res = alloc(ml + mr, n)
    res(0 until ml, ::) := lhs
    res(ml until ml + mr, ::) := rhs
    result(res)
  }

  protected def flattenArray(array: Array[Mat[A]], blockRows: Int, blockCols: Int): MA = {
    def block(br: Int, bc: Int): Mat[A] = array(br + bc * blockRows)
    require(blockRows > 0 && blockCols > 0)
    var rows = 0
    var cols = 0
    cforRange(0 until blockCols) { bc =>
      cols += block(0, bc).cols
    }
    cforRange(0 until blockRows) { br =>
      rows += block(br, 0).rows
    }
    val res = alloc(rows, cols)
    var row = 0
    cforRange(0 until blockRows) { br =>
      var col = 0
      val nr = block(br, 0).rows
      cforRange(0 until blockCols) { bc =>
        val b = block(br, bc)
        require(b.rows == nr)
        res(row until row + b.rows, col until col + b.cols) := b
        col += b.cols
      }
      row += nr
    }
    result(res)
  }

  def flatMap[B](lhs: Mat[B])(f: B => Mat[A]): MA =
    if (lhs.rows == 0 || lhs.cols == 0) sys.error("Cannot flatten matrix with 0 rows or zero cols.")
    else {
      val els = new Array[Mat[A]](lhs.rows * lhs.cols)
      cforRange(0 until lhs.rows) { r =>
        cforRange(0 until lhs.cols) { c =>
          els(r + c * lhs.rows) = f(lhs(r, c))
        }
      }
      flattenArray(els, lhs.rows, lhs.cols)
    }

  def flatten[B <: Mat[A]](lhs: Mat[B]): MA =
    if (lhs.rows == 0 || lhs.cols == 0) sys.error("Cannot flatten matrix with 0 rows or zero cols.")
    else {
      val els = new Array[Mat[A]](lhs.rows * lhs.cols)
      cforRange(0 until lhs.rows) { r =>
        cforRange(0 until lhs.cols) { c =>
          els(r + c * lhs.rows) = lhs(r, c)
        }
      }
      flattenArray(els, lhs.rows, lhs.cols)
    }

}
