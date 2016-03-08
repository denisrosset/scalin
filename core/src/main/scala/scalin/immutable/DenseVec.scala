package scalin
package immutable

class DenseVec[A](val data: Array[AnyRef])
    extends scalin.DenseVec[A] with scalin.immutable.Vec[A]

object DenseVec extends scalin.DenseVecFactory[DenseVec] {

  protected def build[A](data: Array[AnyRef]): DenseVec[A] =
    new DenseVec[A](data)

}
