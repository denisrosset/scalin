package scalin
package generic

import spire.syntax.cfor._
import scalin.syntax.all._

/** A compressed sparse column matrix, as used in Matlab, etc..,
  * see [[https://en.wikipedia.org/wiki/Sparse_matrix#Compressed_sparse_column_.28CSC_or_CCS.29]]
  *
  */
abstract class CSCMat[A] extends scalin.Mat[A] {
  implicit def sparse: Sparse[A]

  protected def data: Array[AnyRef] // data elements, not necessarily nonzero
  protected def colPtrs: Array[Int]
  protected def rowIndices: Array[Int]

  type Ptr <: Long // encoding of col and rowIndex
                   // special value -1L is empty

  def ptr(col: Int): Ptr =
    if (colPtrs(col) == colPtrs(col + 1))
      (-1L).asInstanceOf[Ptr]
    else
      ((col.toLong << 32) + colPtrs(col).toLong).asInstanceOf[Ptr]

  def ptrIndex(ptr: Ptr): Int = {
    require((ptr: Long) != -1L)
    (ptr: Long).toInt
  }
  def ptrRow(ptr: Ptr): Int = {
    require((ptr: Long) != -1L)
    rowIndices(ptrIndex(ptr))
  }
  def ptrCol(ptr: Ptr): Int = {
    require((ptr: Long) != -1L)
    ((ptr: Long) >> 32).toInt
  }
  def ptrIsEmpty(ptr: Ptr): Boolean = (ptr: Long) == -1L
  def ptrNonEmpty(ptr: Ptr): Boolean = (ptr: Long) != -1L
  def ptrNext(ptr: Ptr): Ptr = {
    val c = ptrCol(ptr)
    val newRowIndex = ptrIndex(ptr) + 1
    if (newRowIndex == colPtrs(c + 1))
      (-1L).asInstanceOf[Ptr]
    else
      ((ptr: Long) + 1L).asInstanceOf[Ptr]
  }
  def ptrValue(ptr: Ptr): A = data(ptrIndex(ptr)).asInstanceOf[A]

  protected def locate(row: Int, col: Int): Int = {
    import spire.std.int._
    val start = colPtrs(col)
    val end = colPtrs(col + 1)
    if (start == end)
      ~start
    else
      spire.math.Searching.search(rowIndices, row, start, end - 1)
  }

  def apply(row: Int, col: Int): A = {
    if (row >= nRows || col >= nCols || row < 0 || col < 0)
      throw new IndexOutOfBoundsException()
    val ind = locate(row, col)
    if (ind < 0) sparse.zero
    else data(ind).asInstanceOf[A]
  }
}

abstract class CSCMatEngine[A, +MA <: generic.CSCMat[A]] extends scalin.MatEngine[A, MA] {
  type Mut = mutable.CSCMat[A]

  implicit def sparse: Sparse[A]

  protected def build(nRows: Int, nCols: Int,
                      data: Array[AnyRef],
                      colPtrs: Array[Int],
                      rowIndices: Array[Int]): MA

  protected def empty(nRows: Int, nCols: Int, initialNonzero: Int): MA =
    build(nRows, nCols,
      new Array[AnyRef](initialNonzero),
      new Array[Int](nCols + 1),
      new Array[Int](initialNonzero))

  def tabulate(nRows: Int, nCols: Int)(f: (Int, Int) => A): MA = {
    val mut = mutableEngine.fillConstant(nRows, nCols)(sparse.zero)
    cforRange(0 until nCols) { c =>
      cforRange(0 until nRows) { r =>
        mut(r, c) := f(r, c)
      }
    }
    mutableConv(mut)
  }

  def fillConstant(nRows: Int, nCols: Int)(a: => A): MA = {
    val value = a
    if (sparse.provenZero(a) || nRows == 0 || nCols == 0)
      build(nRows, nCols, new Array[AnyRef](0), new Array[Int](nCols + 1), new Array[Int](0))
    else {
      val colPtrs = Array.tabulate(nCols + 1)(_ * nRows)
      val rowIndices = Array.range(0, nCols).flatMap(c => Array.range(0, nRows))
      val data = new Array[AnyRef](nRows * nCols)
      java.util.Arrays.fill(data, value)
      build(nRows, nCols, data, colPtrs, rowIndices)
    }
  }

}

abstract class CSCMatType[M[A] <: generic.CSCMat[A]] extends scalin.MatType[M] {
  type TC[A] = Sparse[A]
}
