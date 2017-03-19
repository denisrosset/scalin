package scalin
package immutable

class DenseMat[A](val nRows: Int, val nCols: Int, val data: Array[AnyRef])
    extends scalin.DenseMat[A] with scalin.immutable.Mat[A]

object DenseMat extends DenseMatType[immutable.DenseMat] {

  protected def build[A](rows: Int, cols: Int, data: Array[AnyRef]): DenseMat[A] =
    new DenseMat[A](rows, cols, data)

  class Engine[A] extends scalin.MatEngine[A, immutable.DenseMat[A]] {

    def tabulate(nRows: Int, nCols: Int)(f: (Int, Int) => A) = DenseMat.tabulate_[A](nRows, nCols)(f)

    def fromMutable(nRows: Int, nCols: Int, default: A)(updateFun: scalin.mutable.Mat[A] => Unit) = {
      val array = newArray[A](nRows * nCols, default)
      val res = new scalin.mutable.DenseMat[A](nRows, nCols, array)
      updateFun(res)
      res.result()
    }

    def fromMutableUnsafe(nRows: Int, nCols: Int)(updateFun: scalin.mutable.Mat[A] => Unit) = {
      val res = new scalin.mutable.DenseMat[A](nRows, nCols, new Array[AnyRef](nRows * nCols))
      updateFun(res)
      res.result()
    }

  }


  def defaultEngine[A:TC]: scalin.MatEngine[A, immutable.DenseMat[A]] = new Engine[A]

}
