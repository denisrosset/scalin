package scalin
package immutable

case class RowVec[A](col: immutable.Vec[A]) extends scalin.RowVec[A] with Immutable
