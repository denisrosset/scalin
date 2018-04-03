package scalin
package generic

import scalin.mutable.COOMat

import scala.annotation.tailrec

abstract class COOToCSC[A, To <: generic.CSCMat[A]] extends MatConv[A, COOMat[A], To] {
  protected def build(nRows: Int, nCols: Int,
                      data: Array[AnyRef],
                      colPtrs: Array[Int],
                      rowIndices: Array[Int]): To

  def apply(from: COOMat[A]): To = {
    from.shellSort()
    from.compact()
    val data = java.util.Arrays.copyOfRange(from.data, 0, from.n)
    val rowIndices = java.util.Arrays.copyOfRange(from.rowIndices, 0, from.n)
    val colIndices = java.util.Arrays.copyOfRange(from.colIndices, 0, from.n)
    val colPtrs = new Array[Int](from.nCols + 1)
    @tailrec def fillColPtrs(c: Int, // Current column, has been already filled
                     i: Int  // Index of new position to test
                    ): Unit =
      if (c <= from.nCols) {
        colPtrs(c) = i
        if (i == from.n) { // no more elements, colPtrs should be equal to n
          if (c < from.nCols)
            fillColPtrs(c + 1, i)
        }
        else if (c < colIndices(i)) // more elements, but jumped columns
          fillColPtrs(c + 1, i) // catch up the next column
        else if (c == colIndices(i)) // we are on that column
          fillColPtrs(c + 1, i + 1) // go to the next column, this one is fixed
        else // we are already at the next column, but this is a run of elements for the previous column
          fillColPtrs(c, i + 1)
      } else sys.error("Inconsistent data")
    fillColPtrs(0, 0)
    build(from.nRows, from.nCols, data, colPtrs, rowIndices)
  }
}
