package scalin
package mutable

import scalin.generic.DenseMatType

class DenseMat[A](val nRows: Int,
                  val nCols: Int,
                  var data: Array[AnyRef], // mutable data array
                  var sharedData: Boolean // if exported = true, data has to be treated as immutable
                 ) extends generic.DenseMat[A] with mutable.Mat[A] {

  def prepareMutation(): Unit =
    if (sharedData) {
      data = data.clone
      sharedData = false
    }

  def copyIfOverlap(obj: AnyRef): DenseMat[A] = if (obj eq this) new DenseMat[A](nRows, nCols, data.clone, false) else this

  def set(r: Int, c: Int, a: A): Unit = {
    prepareMutation()
    data(r + c * nRows) = a.asInstanceOf[AnyRef]
  }

}

object DenseMat extends DenseMatType[mutable.DenseMat] {

  class Engine[A] extends generic.DenseMatEngine[A, mutable.DenseMat[A]] { self =>
    type Ret = mutable.DenseMat[A]
    def mutableEngine: scalin.MatEngine[A, mutable.DenseMat[A]] = self
    implicit def mutableConv: MatConv[A, mutable.DenseMat[A], mutable.DenseMat[A]] =
      new MatConv[A, mutable.DenseMat[A], mutable.DenseMat[A]] {
        def apply(from: mutable.DenseMat[A]): mutable.DenseMat[A] =
          new mutable.DenseMat[A](from.nRows, from.nCols, from.data.clone, false)
      }

    protected def build(nRows: Int, nCols: Int, data: Array[AnyRef]): DenseMat[A] =
      new DenseMat[A](nRows, nCols, data, false)
  }

  def defaultEngine[A: TC]: scalin.MatEngine[A, mutable.DenseMat[A]] = new Engine[A]

}
