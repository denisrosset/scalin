package scalin
package immutable

trait Vec[A] extends scalin.Vec[A] { lhs =>

  def copyIfOverlap(obj: AnyRef) = lhs // immutable object can never overlap

}

object Vec {

  def tabulate[A](length: Int)( f: Int => A ): immutable.Vec[A] =
    immutable.DenseVec.tabulate[A](length)(f)

}
