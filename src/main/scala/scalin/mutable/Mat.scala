package scalin
package mutable

import spire.algebra._

trait Mat[A] extends scalin.Mat[A] {

  // one row, one column

  def set(r: Int, c: Int, a: A): Unit

  // one row, many columns

}

object Mat extends MatFactory[Mat, Dummy] {

  def tabulate[A:Dummy](rows: Int, cols: Int)( f: (Int, Int) => A ): Mat[A] =
    DenseMat.tabulate[A](rows, cols)(f)

}
