package scalin

import spire.syntax.cfor._

abstract class DenseMat[A] extends scalin.Mat[A] {

  def data: Array[AnyRef]

  def apply(r: Int, c: Int) = {
    require(r >= 0 && c >= 0 && r < nRows && c < nCols)
    data(r + c * nRows).asInstanceOf[A]
  }

}

abstract class DenseMatType[M[A] <: DenseMat[A]] extends MatType[M] {

  type TC[A] = Dummy[A]

  protected def build[A](rows: Int, cols: Int, data: Array[AnyRef]): M[A]

  protected def newArray[A](size: Int, default: A): Array[AnyRef] = {
    val res = new Array[AnyRef](size)
    java.util.Arrays.fill(res, default)
    res
  }

  protected def tabulate_[A](rows: Int, cols: Int)(f: (Int, Int) => A): M[A] = {
    val data = new Array[AnyRef](rows * cols)
    cforRange(0 until rows) { r =>
      cforRange(0 until cols) { c =>
        data(r + c * rows) = f(r, c).asInstanceOf[AnyRef]
      }
    }
    build(rows, cols, data)
  }

}

