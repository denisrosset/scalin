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

object DenseMat extends DenseMatType[mutable.DenseMat] {

  protected def build[A](nRows: Int, nCols: Int, data: Array[AnyRef]): DenseMat[A] =
    new DenseMat[A](nRows, nCols, data)

  class Engine[A] extends scalin.MatEngine[A, mutable.DenseMat[A]] {

    def tabulate(nRows: Int, nCols: Int)(f: (Int, Int) => A) = mutable.DenseMat.tabulate_[A](nRows, nCols)(f)

    def fromMutable(nRows: Int, nCols: Int, default: A)(updateFun: scalin.mutable.Mat[A] => Unit) = {
      val array = newArray[A](nRows * nCols, default)
      val res = new scalin.mutable.DenseMat[A](nRows, nCols, array)
      updateFun(res)
      res
    }

    def fromMutableUnsafe(nRows: Int, nCols: Int)(updateFun: scalin.mutable.Mat[A] => Unit) = {
      val res = new scalin.mutable.DenseMat[A](nRows, nCols, new Array[AnyRef](nRows * nCols))
      updateFun(res)
      res
    }

  }

  def defaultEngine[A:TC]: scalin.MatEngine[A, mutable.DenseMat[A]] = new Engine[A]

}
