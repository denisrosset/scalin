package scalin
package mutable

import scalin.generic
import scalin.generic.DenseVecType

class DenseVec[A](var data: Array[AnyRef], var sharedData: Boolean) extends generic.DenseVec[A] with mutable.Vec[A] {

  def copyIfOverlap(obj: AnyRef) = if (obj eq this) new DenseVec[A](data.clone, false) else this

  def prepareMutation(): Unit = if (sharedData) data = data.clone

  def set(k: Int, a: A): Unit = {
    prepareMutation()
    data(k) = a.asInstanceOf[AnyRef]
  }
}

object DenseVec extends DenseVecType[mutable.DenseVec] with VecType[mutable.DenseVec] {

  class Engine[A] extends generic.DenseVecEngine[A, mutable.DenseVec[A]] { self =>

    type Ret = mutable.DenseVec[A]

    protected def build(data: Array[AnyRef]): mutable.DenseVec[A] = new mutable.DenseVec[A](data, false)

    def mutableEngine: scalin.VecEngine[A, mutable.DenseVec[A]] = self

    implicit def mutableConv: VecConv[A, mutable.DenseVec[A], mutable.DenseVec[A]] =
      new VecConv[A, mutable.DenseVec[A], mutable.DenseVec[A]] {
        def apply(from: mutable.DenseVec[A]): mutable.DenseVec[A] =
          new mutable.DenseVec[A](from.data.clone, false)
      }

    override def fromMutable(length: Int, default: => A)(updateFun: mutable.DenseVec[A] => Unit): mutable.DenseVec[A] = {
      val res = fillConstant(length)(default)
      updateFun(res)
      res
    }
  }

  def defaultEngine[A:TC]: scalin.VecEngine[A, mutable.DenseVec[A]] = new Engine[A]

}
