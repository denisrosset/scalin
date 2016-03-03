package scalin

import spire.syntax.cfor._

abstract class DenseVec[A] extends scalin.Vec[A] {

  def data: Array[AnyRef]

  def length = data.length

  def apply(k: Int) = {
    require(k >= 0 && k < length)
    data(k).asInstanceOf[A]
  }

  type Sliced <: DenseVec[A]
  protected def build(newData: Array[AnyRef]): Sliced

  def apply(ind: Slice): Sliced = {
    val n = ind.length
    val newData = new Array[AnyRef](n)
    cforRange(0 until n) { k =>
      newData(k) = data(ind(k))
    }
    build(newData)
  }

}

abstract class DenseVecFactory[DV[A] <: DenseVec[A]] extends scalin.VecFactory[DV, Dummy] {

  protected def build[A](data: Array[AnyRef]): DV[A]

  def tabulate[A:Dummy](length: Int)(f: Int => A): DV[A] = {
    val data = new Array[AnyRef](length)
    cforRange(0 until length) { k =>
      data(k) = f(k).asInstanceOf[AnyRef]
    }
    build(data)
  }

}
