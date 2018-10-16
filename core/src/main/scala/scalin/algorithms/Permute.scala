package scalin
package algorithms

import scalin.syntax.assign._

import spire.syntax.cfor._

object Permute {

  def permuteInverse[A, VA <: mutable.Vec[A]](v: VA, permInverse: Array[Int]): Unit = {
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

  def rowsPermute[A, MA <: mutable.Mat[A]](m: MA, r1: Int, r2: Int): Unit = {
    cforRange(0 until m.nCols) { c =>
      val t = m(r1, c)
      m(r1, c) := m(r2, c)
      m(r2, c) := t
    }
  }

  def rowsPermuteInverse[A, MA <: mutable.Mat[A]](m: MA, rowPermInverse: Array[Int]): Unit = {
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

  def colsPermuteInverse[A, MA <: mutable.Mat[A]](m: MA, colPermInverse: Array[Int]): Unit = {
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


}
