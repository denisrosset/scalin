package scalin
package immutable

trait DenseOps extends scalin.algebra.ops[immutable.Vec, immutable.Mat] {

  def defaultTabulate[A](length: Int)(f: Int => A): immutable.Vec[A] =
    immutable.DenseVec.tabulate(length)(f)

  def defaultTabulate[A](rows: Int, cols: Int)(f: (Int, Int) => A): immutable.Mat[A] =
    immutable.DenseMat.tabulate(rows, cols)(f)

}

object dense extends DenseOps
