package scalin
package generic

import spire.syntax.cfor._

abstract class DenseMat[A] extends scalin.Mat[A] {

  def data: Array[AnyRef]

  def apply(r: Int, c: Int) = {
    require(r >= 0 && c >= 0 && r < nRows && c < nCols)
    data(r + c * nRows).asInstanceOf[A]
  }

}

abstract class DenseMatEngine[A, +MA <: DenseMat[A]] extends scalin.MatEngine[A, MA] {
  type Mut = mutable.DenseMat[A]

  protected def newArray(size: Int, default: => A): Array[AnyRef] = {
    val res = new Array[AnyRef](size)
    if (size > 0)
      java.util.Arrays.fill(res, default)
    res
  }

  protected def build(nRows: Int, nCols: Int, data: Array[AnyRef]): MA

  def fillConstant(nRows: Int, nCols: Int)(a: => A): MA =
    build(nRows, nCols, newArray(nRows * nCols, a))

  def tabulate(rows: Int, cols: Int)(f: (Int, Int) => A): MA = {
    val data = new Array[AnyRef](rows * cols)
    cforRange(0 until rows) { r =>
      cforRange(0 until cols) { c =>
        data(r + c * rows) = f(r, c).asInstanceOf[AnyRef]
      }
    }
    build(rows, cols, data)
  }
}

abstract class DenseMatType[M[A] <: DenseMat[A]] extends scalin.MatType[M] {

  type TC[A] = Dummy[A]

}
