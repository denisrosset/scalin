package scalin
package mutable

object ops extends scalin.algebra.ops[mutable.Vec, mutable.Mat] {

  import algebra._
  import spire.algebra._

  def defaultTabulate[A](length: Int)(f: Int => A): mutable.Vec[A] =
    mutable.DenseVec.tabulate(length)(f)

  def defaultTabulate[A](rows: Int, cols: Int)(f: (Int, Int) => A): mutable.Mat[A] =
    mutable.DenseMat.tabulate(rows, cols)(f)

}
