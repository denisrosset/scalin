package scalin
package mutable

import scala.annotation.tailrec

class DOKToCOO[A:SparseAdditiveGroup] extends MatConv[A, DOKMat[A], COOMat[A]] {
  def apply(from: DOKMat[A]): COOMat[A] = {
    val n = from.data.size
    val data = new Array[AnyRef](n)
    val rowIndices = new Array[Int](n)
    val colIndices = new Array[Int](n)
    @tailrec def rec(i: Int, it: Iterator[Long]): Unit =
      if (it.hasNext) {
        val index = it.next()
        data(i) = from.data(index)
        rowIndices(i) = DOKMat.rowFromIndex(index)
        colIndices(i) = DOKMat.colFromIndex(index)
        rec(i, it)
      }
    rec(0, from.data.keysIterator)
    new COOMat[A](from.nRows, from.nCols, data, rowIndices, colIndices, n)
  }
}
