package scalin
package immutable

trait RowVec[A, +V <: immutable.Vec[A]] extends scalin.RowVec[A, V] with Immutable

object RowVec {

  def apply[A, V <: immutable.Vec[A]](v: V): immutable.RowVec[A, V] = new immutable.RowVec[A, V] {
    val t = v
  }

}
