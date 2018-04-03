package scalin
package immutable

import scalin.generic.COOToCSC
import scalin.mutable.{COOMat, DOKMat}

class CSCMat[A](val nRows: Int, val nCols: Int,
                val data: Array[AnyRef],
                val colPtrs: Array[Int],
                val rowIndices: Array[Int])
               (implicit val sparse: Sparse[A]) extends generic.CSCMat[A] with immutable.Mat[A]

object CSCMat extends generic.CSCMatType[immutable.CSCMat] {

  class Engine[A](implicit val sparse: Sparse[A]) extends scalin.generic.CSCMatEngine[A, immutable.CSCMat[A]] {
    type Ret = immutable.CSCMat[A]
    def mutableEngine: scalin.MatEngine[A, mutable.CSCMat[A]] = mutable.CSCMat.defaultEngine
    def mutableConv: MatConv[A, mutable.CSCMat[A], immutable.CSCMat[A]] = CSCMat.fromMutable[A]

    protected def build(nRows: Int, nCols: Int,
                        data: Array[AnyRef],
                        colPtrs: Array[Int],
                        rowIndices: Array[Int]): CSCMat[A] =
      new CSCMat[A](nRows, nCols, data, colPtrs, rowIndices)
  }

  def defaultEngine[A:Sparse]: scalin.MatEngine[A, immutable.CSCMat[A]] = new Engine[A]

  implicit def fromCOO[A:Sparse]: MatConv[A, COOMat[A], immutable.CSCMat[A]] = new COOToCSC[A, immutable.CSCMat[A]] {
    protected def build(nRows: Int, nCols: Int,
                        data: Array[AnyRef], colPtrs: Array[Int], rowIndices: Array[Int]): CSCMat[A] =
      new CSCMat[A](nRows, nCols, data, colPtrs, rowIndices)
  }
  implicit def fromDOK[A:SparseAdditiveGroup]: MatConv[A, DOKMat[A], immutable.CSCMat[A]] = DOKMat.toCOO[A].andThen(fromCOO[A])

  private[this] object FromMutable extends MatConv[Any, mutable.CSCMat[Any], immutable.CSCMat[Any]] {
    def apply(from: mutable.CSCMat[Any]): immutable.CSCMat[Any] = {
      val n = from.colPtrs(from.nCols)
      val data = java.util.Arrays.copyOf(from.data, n)
      val colPtrs = from.colPtrs.clone
      val rowIndices = java.util.Arrays.copyOf(from.rowIndices, n)
      new immutable.CSCMat[Any](from.nRows, from.nCols, data, colPtrs, rowIndices)(from.sparse)
    }
  }

  implicit def fromMutable[A]: MatConv[A, mutable.CSCMat[A], immutable.CSCMat[A]] =
    FromMutable.asInstanceOf[MatConv[A, mutable.CSCMat[A], immutable.CSCMat[A]]]
}
