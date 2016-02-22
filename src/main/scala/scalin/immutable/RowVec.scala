package scalin
package immutable

case class RowVec[A](col: immutable.Vec[A]) extends scalin.RowVec[A] with Immutable

object RowVec {

  implicit def fromVec[A](v: immutable.Vec[A]): immutable.RowVec[A] = apply(v)

}
