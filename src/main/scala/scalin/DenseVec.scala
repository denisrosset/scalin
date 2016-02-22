package scalin

import spire.syntax.cfor._

abstract class DenseVec[A] extends scalin.Vec[A] {

  type AsImmutable = immutable.DenseVec[A]
  type AsMutable = mutable.DenseVec[A]

  def nextNonZero(k: Int = -1): Int = k + 1

  def data: Array[AnyRef]

  def length = data.length

  def apply(k: Int) = data(k).asInstanceOf[A]

  def mutableCopy: AsMutable = new mutable.DenseVec[A](data.clone)

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
