package scalin
package immutable

trait Mat[A] extends scalin.Mat[A] { lhs =>

  def copyIfOverlap(obj: AnyRef) = lhs // immutable object can never overlap

}

object Mat extends MatFactory[Mat, Dummy] {

  def tabulate[A:Dummy](rows: Int, cols: Int)( f: (Int, Int) => A ): Mat[A] =
    DenseMat.tabulate[A](rows, cols)(f)

}
