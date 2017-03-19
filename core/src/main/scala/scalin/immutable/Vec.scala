package scalin
package immutable

trait Vec[A] extends scalin.Vec[A] { lhs =>

  def copyIfOverlap(obj: AnyRef) = lhs // immutable object can never overlap

}

object Vec extends VecType[scalin.immutable.Vec] {

  def defaultEngine[A:TC] = DenseVec.defaultEngine[A]

}
