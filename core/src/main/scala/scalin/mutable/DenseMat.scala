package scalin
package mutable

class DenseMat[A](val nRows: Int, val nCols: Int, var data: Array[AnyRef]) extends scalin.DenseMat[A] with mutable.Mat[A] {

  def copyIfOverlap(obj: AnyRef) = if (obj eq this) new DenseMat[A](nRows, nCols, data.clone) else this

  def set(r: Int, c: Int, a: A): Unit = {
    data(r + c * nRows) = a.asInstanceOf[AnyRef]
  }

  def exported = (data eq null)

  def result(): immutable.DenseMat[A] = {
    val res = new immutable.DenseMat[A](nRows, nCols, data)
    data = null
    res
  }

}

object DenseMat extends scalin.DenseMatFactory[DenseMat] {

  def build[A](rows: Int, cols: Int, data: Array[AnyRef]): DenseMat[A] =
    new DenseMat[A](rows, cols, data)

}
