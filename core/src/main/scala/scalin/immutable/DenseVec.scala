package scalin
package immutable

import scalin.generic
import scalin.generic.DenseVecType

class DenseVec[A](val data: Array[AnyRef])
    extends generic.DenseVec[A] with scalin.immutable.Vec[A]

object DenseVec extends DenseVecType[DenseVec] {

  protected def build[A](data: Array[AnyRef]): DenseVec[A] = new DenseVec[A](data)

  class Engine[A] extends generic.DenseVecEngine[A, immutable.DenseVec[A]] {

    type Ret = immutable.DenseVec[A]

    protected def build(data: Array[AnyRef]): DenseVec[A] = new DenseVec[A](data)

    def mutableEngine: scalin.VecEngine[A, mutable.DenseVec[A]] = mutable.DenseVec.defaultEngine

    implicit def mutableConv: VecConv[A, mutable.DenseVec[A], immutable.DenseVec[A]] =
      new VecConv[A, mutable.DenseVec[A], immutable.DenseVec[A]] {
        def apply(from: mutable.DenseVec[A]): DenseVec[A] = {
          from.sharedData = true
          new immutable.DenseVec[A](from.data)
        }
      }
  }

  def defaultEngine[A:Dummy]: scalin.VecEngine[A, DenseVec[A]] = new Engine[A]

}
