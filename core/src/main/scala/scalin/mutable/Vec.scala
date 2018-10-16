package scalin
package mutable

import spire.algebra.AdditiveSemigroup
import spire.syntax.cfor._

trait Vec[A] extends scalin.Vec[A] { lhs =>

  def sharedData: Boolean

  def prepareMutation(): Unit

  def set(k: Int, a: A): Unit

  // useful for sparse vectors that can have duplicate entries
  def add(k: Int, a: A)(implicit ev: AdditiveSemigroup[A]): Unit = set(k, ev.plus(apply(k), a))

  def set(sub: Subscript, rhs: A): Unit = {
    val ind = sub.forLength(length)
    cforRange(0 until ind.length) { k =>
      set(ind(k), rhs)
    }
  }

  def set(sub: Subscript, rhs: scalin.Vec[A]): Unit = {
    val ind = sub.forLength(length)
    val rhsCopy = rhs.copyIfOverlap(lhs)
    val n = ind.length
    require(n == rhsCopy.length)
    cforRange(0 until n) { k =>
      set(ind(k), rhsCopy(k))
    }
  }

}

object Vec extends VecType[scalin.mutable.Vec] {

  type TC[A] = Dummy[A]

  def defaultEngine[A:TC]: scalin.VecEngine[A, DenseVec[A]] = DenseVec.defaultEngine[A]

}
