package scalin
package mutable

trait DenseOps extends scalin.algebra.ops[mutable.Vec, mutable.Mat] {

  def defaultTabulate[A](length: Int)(f: Int => A): mutable.Vec[A] =
    mutable.DenseVec.tabulate(length)(f)

  def defaultTabulate[A](rows: Int, cols: Int)(f: (Int, Int) => A): mutable.Mat[A] =
    mutable.DenseMat.tabulate(rows, cols)(f)

}

object dense extends DenseOps
