package scalin
package immutable

trait Mat[A] extends scalin.Mat[A]

object Mat extends MatFactory[Mat, Dummy] {

  def tabulate[A:Dummy](rows: Int, cols: Int)( f: (Int, Int) => A ): Mat[A] =
    DenseMat.tabulate[A](rows, cols)(f)

}
