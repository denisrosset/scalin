package scalin
package immutable

trait Mat[A] extends scalin.Mat[A] { lhs =>

  def copyIfOverlap(obj: AnyRef) = lhs // immutable object can never overlap

}
