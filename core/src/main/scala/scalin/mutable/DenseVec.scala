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

object DenseVec extends DenseVecType[mutable.DenseVec] with VecType[mutable.DenseVec] {

  protected def build[A](data: Array[AnyRef]): mutable.DenseVec[A] = new mutable.DenseVec[A](data)

  class Engine[A] extends VecEngine[A, mutable.DenseVec[A]] {

    def tabulate(length: Int)(f: Int => A) = tabulate_[A](length)(f)

    def fromMutable(length: Int, default: A)(updateFun: scalin.mutable.Vec[A] => Unit) = {
      val array = newArray[A](length, default)
      val res = new scalin.mutable.DenseVec[A](array) // add method to mutable VecEngine to allocate vector with null, sound semantics; see mutable.VecEngine as well
      updateFun(res)
      res
    }

    def fromMutableUnsafe(length: Int)(updateFun: scalin.mutable.Vec[A] => Unit) = {
      val res = new scalin.mutable.DenseVec[A](new Array[AnyRef](length)) // add method to mutable VecEngine to allocate vector with null, sound semantics; see mutable.VecEngine as well
      updateFun(res)
      res
    }

  }


  def engine[A:TC] = new Engine[A]

}
