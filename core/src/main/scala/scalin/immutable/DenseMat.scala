package scalin
package immutable

class DenseMat[A](val nRows: Int, val nCols: Int, val data: Array[AnyRef])
    extends scalin.DenseMat[A] with scalin.immutable.Mat[A]

object DenseMat extends scalin.DenseMatFactory[DenseMat] {

  protected def build[A](rows: Int, cols: Int, data: Array[AnyRef]): DenseMat[A] =
    new DenseMat[A](rows, cols, data)

}
