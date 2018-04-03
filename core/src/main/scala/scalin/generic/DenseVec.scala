package scalin
package generic

import scalin.{Dummy, VecType}
import spire.syntax.cfor._

abstract class DenseVec[A] extends scalin.Vec[A] {

  def data: Array[AnyRef]

  def length = data.length

  def apply(k: Int) = {
    require(k >= 0 && k < length)
    data(k).asInstanceOf[A]
  }

}

abstract class DenseVecEngine[A, +VA <: DenseVec[A]] extends scalin.VecEngine[A, VA] {
  type Mut = mutable.DenseVec[A]

  protected def newArray(size: Int, default: => A): Array[AnyRef] = {
    val res = new Array[AnyRef](size)
    if (size > 0)
      java.util.Arrays.fill(res, default)
    res
  }

  protected def build(data: Array[AnyRef]): VA

  def fillConstant(length: Int)(a: => A): VA =
    build(newArray(length, a))

  def tabulate(length: Int)(f: Int => A): VA = {
    val data = new Array[AnyRef](length)
    cforRange(0 until length) { i =>
      data(i) = f(i).asInstanceOf[AnyRef]
    }
    build(data)
  }
}

abstract class DenseVecType[V[A] <: DenseVec[A]] extends VecType[V] {

  type TC[A] = Dummy[A]

}
