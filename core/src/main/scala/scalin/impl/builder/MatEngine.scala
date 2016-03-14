package scalin
package impl
package builder

import scalin.syntax.assign._

import spire.syntax.cfor._

trait MatEngine[A, MA <: Mat[A]] extends scalin.impl.MatEngine[A, MA] {

  //// Mutable variant

  type UMA <: scalin.mutable.Mat[A]
  implicit def UMA: scalin.algebra.MatEngine[A, UMA]

  type UVA <: scalin.mutable.Vec[A]
  implicit def UVA: scalin.algebra.VecEngine[A, UVA]

  def rowsPermute(m: UMA, r1: Int, r2: Int): Unit = {
    cforRange(0 until m.nCols) { c =>
      val t = m(r1, c)
      m(r1, c) := m(r2, c)
      m(r2, c) := t
    }
  }

  def permuteInverse(v: UVA, permInverse: Array[Int]): Unit = {
    val bs = scala.collection.mutable.BitSet.empty
    cforRange(0 until v.length) { i => bs += i }
    while (bs.nonEmpty) {
      val start = bs.head
      bs -= start
      if (start != permInverse(start)) {
        cforRange(0 until v.length) { k =>
          var last = start
          var current = permInverse(start)
          val temp = v(start)
          while (current != start) {
            bs -= current
            v(last) := v(current)
            last = current
            current = permInverse(current)
          }
          v(last) := temp
        }
      }
    }
  }

  def rowsPermuteInverse(m: UMA, rowPermInverse: Array[Int]): Unit = {
    if (m.nCols == 0) return
    val bs = scala.collection.mutable.BitSet.empty
    cforRange(0 until m.nRows) { i => bs += i }
    while (bs.nonEmpty) {
      val start = bs.head
      bs -= start
      if (start != rowPermInverse(start)) {
        cforRange(0 until m.nCols) { c =>
          var last = start
          var current = rowPermInverse(start)
          val temp = m(start, c)
          while (current != start) {
            if (c == 0) bs -= current
            m(last, c) := m(current, c)
            last = current
            current = rowPermInverse(current)
          }
          m(last, c) := temp
        }
      }
    }
  }

  def colsPermuteInverse(m: UMA, colPermInverse: Array[Int]): Unit = {
    if (m.nRows == 0) return
    val bs = scala.collection.mutable.BitSet.empty
    cforRange(0 until m.nCols) { i => bs += i }
    while (bs.nonEmpty) {
      val start: Int = bs.head
      bs -= start
      if (start != colPermInverse(start)) {
        cforRange(0 until m.nRows) { r =>
          var last: Int = start
          var current: Int = colPermInverse(start)
          val temp = m(r, start)
          while (current != start) {
            if (r == 0) bs -= current
            m(r, last) := m(r, current)
            last = current
            current = colPermInverse(current)
          }
          m(r, last) := temp
        }
      }
    }
  }


  /** Returns a mutable vector of the given shape. The initial content of the vector is undefined. */
  def alloc(length: Int): UVA

  /** Returns a mutable matrix of the given shape. The initial content of the matrix is undefined. */
  def alloc(rows: Int, cols: Int): UMA

  /** Converts the mutable matrix `mutable` into the desired instance. `mutable` is destroyed in
    * the process. */
  def result(mutable: UMA): MA

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
    val m = lhs.nRows
    require(m == rhs.nRows)
    val nl = lhs.nCols
    val nr = rhs.nCols
    val res = alloc(m, nl + nr)
    res(::, 0 until nl) := lhs
    res(::, nl until nl + nr) := rhs
    result(res)
  }

  def vertcat(lhs: Mat[A], rhs: Mat[A]): MA = {
    val n = lhs.nCols
    require(n == rhs.nCols)
    val ml = lhs.nRows
    val mr = rhs.nRows
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
      cols += block(0, bc).nCols
    }
    cforRange(0 until blockRows) { br =>
      rows += block(br, 0).nRows
    }
    val res = alloc(rows, cols)
    var row = 0
    cforRange(0 until blockRows) { br =>
      var col = 0
      val nr = block(br, 0).nRows
      cforRange(0 until blockCols) { bc =>
        val b = block(br, bc)
        require(b.nRows == nr)
        res(row until row + b.nRows, col until col + b.nCols) := b
        col += b.nCols
      }
      row += nr
    }
    result(res)
  }

  def flatMap[B](lhs: Mat[B])(f: B => Mat[A]): MA =
    if (lhs.nRows == 0 || lhs.nCols == 0) sys.error("Cannot flatten matrix with 0 rows or zero cols.")
    else {
      val els = new Array[Mat[A]](lhs.nRows * lhs.nCols)
      cforRange(0 until lhs.nRows) { r =>
        cforRange(0 until lhs.nCols) { c =>
          els(r + c * lhs.nRows) = f(lhs(r, c))
        }
      }
      flattenArray(els, lhs.nRows, lhs.nCols)
    }

  def flatten[B <: Mat[A]](lhs: Mat[B]): MA =
    if (lhs.nRows == 0 || lhs.nCols == 0) sys.error("Cannot flatten matrix with 0 rows or zero cols.")
    else {
      val els = new Array[Mat[A]](lhs.nRows * lhs.nCols)
      cforRange(0 until lhs.nRows) { r =>
        cforRange(0 until lhs.nCols) { c =>
          els(r + c * lhs.nRows) = lhs(r, c)
        }
      }
      flattenArray(els, lhs.nRows, lhs.nCols)
    }

}
