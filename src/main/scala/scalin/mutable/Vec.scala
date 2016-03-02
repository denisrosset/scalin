package scalin
package mutable

import spire.algebra._
import spire.syntax.cfor._

trait Vec[A] extends scalin.Vec[A] { lhs =>

  def set(k: Int, a: A): Unit

  def setPlus(k: Int, a: A)(implicit A: AdditiveSemigroup[A]): Unit = set(k, A.plus(apply(k), a))

  def setPlus(rhs: scalin.Vec[A])(implicit A: AdditiveSemigroup[A]): Unit = {
    require(lhs.length == rhs.length)
    cforRange(0 until length) { k =>

//      set(k, A.plus(lhs.apply(k), rhs.apply(k)))
    }
  }

}

object Vec extends VecFactory[Vec, Dummy] {

  def tabulate[A:Dummy](length: Int)( f: Int => A ): Vec[A] =
    DenseVec.tabulate[A](length)(f)

}
