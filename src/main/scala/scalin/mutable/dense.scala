package scalin
package mutable

trait DenseOps extends scalin.algebra.ops[mutable.DenseVec, mutable.DenseMat] {

  def defaultTabulate[A](length: Int)(f: Int => A): mutable.DenseVec[A] =
    mutable.DenseVec.tabulate(length)(f)

  def defaultTabulate[A](rows: Int, cols: Int)(f: (Int, Int) => A): mutable.DenseMat[A] =
    mutable.DenseMat.tabulate(rows, cols)(f)

}

object dense extends DenseOps
