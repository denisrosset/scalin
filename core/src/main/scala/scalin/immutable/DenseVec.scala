package scalin
package immutable

class DenseVec[A](val data: Array[AnyRef])
    extends scalin.DenseVec[A] with scalin.immutable.Vec[A]

object DenseVec extends DenseVecType[DenseVec] {

  protected def build[A](data: Array[AnyRef]): DenseVec[A] = new DenseVec[A](data)

  class Engine[A] extends scalin.VecEngine[A, immutable.DenseVec[A]] {

    def tabulate(length: Int)(f: Int => A) = tabulate_[A](length)(f)

    def fromMutable(length: Int, default: A)(updateFun: scalin.mutable.Vec[A] => Unit) = {
      val array = newArray[A](length, default)
      val res = new scalin.mutable.DenseVec[A](array) // add method to mutable VecEngine to allocate vector with null, sound semantics; see mutable.VecEngine as well
      updateFun(res)
      res.result()
    }

    def fromMutableUnsafe(length: Int)(updateFun: scalin.mutable.Vec[A] => Unit) = {
      val res = new scalin.mutable.DenseVec[A](new Array[AnyRef](length)) // add method to mutable VecEngine to allocate vector with null, sound semantics; see mutable.VecEngine as well
      updateFun(res)
      res.result()
    }

  }

  def engine[A:Dummy]: scalin.VecEngine[A, DenseVec[A]] = new Engine[A]

}
