package scalin
package mutable

import spire.syntax.cfor._

class DenseVec[A](var data: Array[AnyRef]) extends scalin.DenseVec[A] with mutable.Vec[A] with Builder {

  def update(k: Int, a: A): Unit = {
    data(k) = a.asInstanceOf[AnyRef]
  }

  def toImmutable: AsImmutable = new immutable.DenseVec[A](data.clone)

  def exported = (data eq null)

  def result(): AsImmutable = {
    val res = new immutable.DenseVec[A](data)
    data = null
    res
  }

}

object DenseVec extends scalin.DenseVecFactory[mutable.DenseVec] {

  def build[A](data: Array[AnyRef]): mutable.DenseVec[A] =
    new mutable.DenseVec[A](data)

}
