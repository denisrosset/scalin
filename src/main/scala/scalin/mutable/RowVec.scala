package scalin
package mutable

import spire.algebra._

case class RowVec[A](col: mutable.Vec[A]) extends scalin.RowVec[A] with Mutable { lhs =>

  def update(k: Int, a: A): Unit = {
    col(k) = a
  }

  def :=(rhs: AbstractRowVec[A]): Unit = {
    col := rhs.t
  }

}
