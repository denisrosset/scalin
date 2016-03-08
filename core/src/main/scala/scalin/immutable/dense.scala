package scalin
package immutable

trait DenseOps extends scalin.algebra.ops[immutable.DenseVec, immutable.DenseMat] {

  def defaultTabulate[A](length: Int)(f: Int => A): immutable.DenseVec[A] =
    immutable.DenseVec.tabulate(length)(f)

  def defaultTabulate[A](rows: Int, cols: Int)(f: (Int, Int) => A): immutable.DenseMat[A] =
    immutable.DenseMat.tabulate(rows, cols)(f)

}

object dense extends DenseOps
