package scalin
package immutable

import scalin.generic
import scalin.generic.DenseMatType

class DenseMat[A](val nRows: Int, val nCols: Int, val data: Array[AnyRef])
    extends generic.DenseMat[A] with immutable.Mat[A]

object DenseMat extends DenseMatType[immutable.DenseMat] {

  class Engine[A] extends scalin.generic.DenseMatEngine[A, immutable.DenseMat[A]] {
    type Ret = immutable.DenseMat[A]
    def mutableEngine: scalin.MatEngine[A, mutable.DenseMat[A]] = mutable.DenseMat.defaultEngine
    implicit def mutableConv: MatConv[A, mutable.DenseMat[A], DenseMat[A]] = DenseMat.fromMutable[A]
    protected def build(rows: Int, cols: Int, data: Array[AnyRef]): DenseMat[A] = new DenseMat[A](rows, cols, data)
  }

  def defaultEngine[A:TC]: scalin.MatEngine[A, immutable.DenseMat[A]] = new Engine[A]

  private[this] object FromMutable extends MatConv[Any, mutable.DenseMat[Any], immutable.DenseMat[Any]] {
    def apply(from: mutable.DenseMat[Any]): immutable.DenseMat[Any] =
      new immutable.DenseMat[Any](from.nRows, from.nCols, from.data.clone)
  }

  implicit def fromMutable[A]: MatConv[A, mutable.DenseMat[A], immutable.DenseMat[A]] =
    FromMutable.asInstanceOf[MatConv[A, mutable.DenseMat[A], immutable.DenseMat[A]]]
}
