package scalin

abstract class DenseMat[A] extends scalin.Mat[A] {

  def data: Array[AnyRef]

  def apply(r: Int, c: Int) = {
    require(r >= 0 && c >= 0 && r < nRows && c < nCols)
    data(r + c * nRows).asInstanceOf[A]
  }

}
