package scalin

import spire.syntax.cfor._

abstract class DenseVec[A] extends scalin.Vec[A] {

  def data: Array[AnyRef]

  def length = data.length

  def apply(k: Int) = {
    require(k >= 0 && k < length)
    data(k).asInstanceOf[A]
  }

}

abstract class DenseVecFactory[DV[A] <: DenseVec[A]] extends VecType[DV] {

  protected def build[A](data: Array[AnyRef]): DV[A]

  def tabulate[A](length: Int)(f: Int => A)(implicit ev: Dummy[A]): DV[A] = {
    val data = new Array[AnyRef](length)
    cforRange(0 until length) { k =>
      data(k) = f(k).asInstanceOf[AnyRef]
    }
    build(data)
  }

}
