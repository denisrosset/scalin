package scalin

trait DenseOps extends algebra.ops[scalin.DenseVec, scalin.DenseMat] {

  def defaultTabulate[A](length: Int)(f: Int => A): scalin.DenseVec[A] =
    scalin.immutable.DenseVec.tabulate[A](length)(f)

  def defaultTabulate[A](rows: Int, cols: Int)(f: (Int, Int) => A): scalin.DenseMat[A] =
    scalin.immutable.DenseMat.tabulate[A](rows, cols)(f)

}

object dense extends DenseOps
