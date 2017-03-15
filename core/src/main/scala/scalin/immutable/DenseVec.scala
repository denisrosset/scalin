package scalin
package immutable
import scalin.algebra.VecEngine

class DenseVec[A](val data: Array[AnyRef])
    extends scalin.DenseVec[A] with scalin.immutable.Vec[A]

object DenseVec extends scalin.DenseVecFactory[DenseVec] with VecType[DenseVec] {

  type TC[X] = Dummy[X]

  def engine[A:Dummy]: scalin.algebra.VecEngine[A, DenseVec[A]] = dense.vecEngine[A]

  protected def build[A](data: Array[AnyRef]): DenseVec[A] =
    new DenseVec[A](data)

}
