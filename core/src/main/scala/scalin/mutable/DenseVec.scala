package scalin
package mutable

class DenseVec[A](var data: Array[AnyRef]) extends scalin.DenseVec[A] with mutable.Vec[A] {

  def copyIfOverlap(obj: AnyRef) = if (obj eq this) new DenseVec[A](data.clone) else this

  def set(k: Int, a: A): Unit = {
    data(k) = a.asInstanceOf[AnyRef]
  }

  def exported = (data eq null)

  def result(): immutable.DenseVec[A] = {
    val res = new immutable.DenseVec[A](data)
    data = null
    res
  }

}

object DenseVec extends scalin.DenseVecFactory[mutable.DenseVec] {

  def build[A](data: Array[AnyRef]): mutable.DenseVec[A] =
    new mutable.DenseVec[A](data)

}
