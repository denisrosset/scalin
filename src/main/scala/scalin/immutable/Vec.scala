package scalin
package immutable

trait Vec[A] extends scalin.Vec[A] { lhs =>

  def copyIfOverlap(obj: AnyRef) = lhs // immutable object can never overlap

}
