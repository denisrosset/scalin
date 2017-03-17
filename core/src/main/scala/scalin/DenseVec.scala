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

abstract class DenseVecType[V[A] <: DenseVec[A]] extends VecType[V] {

  type TC[A] = Dummy[A]

  protected def build[A](data: Array[AnyRef]): V[A]

  protected def newArray[A](size: Int, default: A): Array[AnyRef] = {
    val res = new Array[AnyRef](size)
    java.util.Arrays.fill(res, default)
    res
  }

  def tabulate_[A](length: Int)(f: Int => A): V[A] = {
    val data = new Array[AnyRef](length)
    cforRange(0 until length) { k =>
      data(k) = f(k).asInstanceOf[AnyRef]
    }
    build(data)
  }

}
