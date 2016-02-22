package scalin
package mutable

import spire.algebra._

trait RowVec[A, +V <: mutable.Vec[A]] extends scalin.RowVec[A, V] with Mutable { lhs =>

  def update(k: Int, a: A): Unit = {
    t(k) = a
  }

  def :=(rhs: AbstractRowVec[A]): Unit = {
    t := rhs.t
  }

}

object RowVec {

  def apply[A, V <: mutable.Vec[A]](v: V): mutable.RowVec[A, V] = new mutable.RowVec[A, V] {
    val t = v
  }

}
