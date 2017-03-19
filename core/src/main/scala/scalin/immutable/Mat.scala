package scalin
package immutable

trait Mat[A] extends scalin.Mat[A] { lhs =>

  def copyIfOverlap(obj: AnyRef) = lhs // immutable object can never overlap

}

object Mat extends MatType[scalin.immutable.Mat] {

  def defaultEngine[A:TC] = DenseMat.defaultEngine[A]

}
