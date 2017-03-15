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

}
