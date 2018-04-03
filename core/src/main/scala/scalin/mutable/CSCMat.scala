package scalin
package mutable

import scalin.generic.COOToCSC
import spire.syntax.cfor._

class CSCMat[A](val nRows: Int, val nCols: Int,
                var data: Array[AnyRef],
                var colPtrs: Array[Int],
                var rowIndices: Array[Int],
                var sharedData: Boolean)
               (implicit val sparse: Sparse[A]) extends generic.CSCMat[A] with mutable.Mat[A] {

  type Ptr = Long

  def used: Int = colPtrs(nCols)

  /** If this matrix has been exported, makes a fresh copy of its data before mutating anything. */
  def prepareMutation(): Unit =
    if (sharedData) {
      data = data.clone()
      colPtrs = colPtrs.clone()
      rowIndices = rowIndices.clone()
    }

  def set(r: Int, c: Int, a: A): Unit = {
    if (r >= nRows || c >= nCols || r < 0 || c < 0)
      throw new IndexOutOfBoundsException()
    prepareMutation()
    val ind = locate(r, c)
    if (ind >= 0) data(ind) = a.asInstanceOf[AnyRef]
    else if (!sparse.provenZero(a)) {
      val insertPos = ~ind
      colPtrs(nCols) += 1

      if (used > data.length) {
        // need to grow array
        val newL = newLength(data.length)

        // allocate new arrays
        val newRowIndices = java.util.Arrays.copyOf(rowIndices, newL)
        val newData = java.util.Arrays.copyOf(data, newL)

        // copy existing data into new arrays
        System.arraycopy(rowIndices, insertPos, newRowIndices, insertPos + 1, used - insertPos - 1)
        System.arraycopy(data, insertPos, newData, insertPos + 1, used - insertPos - 1)

        rowIndices = newRowIndices
        data = newData
      } else if (used - insertPos > 1) {
        // need to make room for new element mid-array
        System.arraycopy(rowIndices, insertPos, rowIndices, insertPos + 1, used - insertPos - 1)
        System.arraycopy(data, insertPos, data, insertPos + 1, used - insertPos - 1)
      }

      // assign new value
      rowIndices(insertPos) = r
      data(insertPos) = a.asInstanceOf[AnyRef]
      cforRange((c + 1) until nCols) { i => // colPtrs(nCols) has already been updated
        colPtrs(i) += 1
      }
    }
  }

  def copyIfOverlap(obj: AnyRef): scalin.Mat[A] = new CSCMat[A](nRows, nCols, data.clone, rowIndices.clone, colPtrs.clone, false)
}

object CSCMat extends generic.CSCMatType[mutable.CSCMat] {

  class Engine[A](implicit val sparse: Sparse[A]) extends scalin.generic.CSCMatEngine[A, mutable.CSCMat[A]] {
    self =>
    type Ret = mutable.CSCMat[A]

    def mutableEngine: scalin.MatEngine[A, CSCMat[A]] = self
    def mutableConv: MatConv[A, mutable.CSCMat[A], mutable.CSCMat[A]] = new MatConv[A, mutable.CSCMat[A], mutable.CSCMat[A]] {
      def apply(from: mutable.CSCMat[A]): mutable.CSCMat[A] =
        new mutable.CSCMat[A](from.nRows, from.nCols, from.data.clone, from.colPtrs.clone, from.rowIndices.clone, false)
    }

    protected def build(nRows: Int, nCols: Int,
                        data: Array[AnyRef],
                        colPtrs: Array[Int],
                        rowIndices: Array[Int]): CSCMat[A] = new CSCMat[A](nRows, nCols, data, colPtrs, rowIndices, false)
  }

  def defaultEngine[A: Sparse]: scalin.MatEngine[A, mutable.CSCMat[A]] = new Engine[A]

  implicit def fromCOO[A:Sparse]: MatConv[A, COOMat[A], mutable.CSCMat[A]] = new COOToCSC[A, mutable.CSCMat[A]] {
    protected def build(nRows: Int, nCols: Int,
                        data: Array[AnyRef], colPtrs: Array[Int], rowIndices: Array[Int]): CSCMat[A] =
      new CSCMat[A](nRows, nCols, data, colPtrs, rowIndices, false)
  }

  implicit def fromDOK[A:SparseAdditiveGroup]: MatConv[A, DOKMat[A], mutable.CSCMat[A]] = DOKMat.toCOO[A].andThen(fromCOO[A])
}
