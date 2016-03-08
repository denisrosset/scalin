package scalin

trait DenseOps extends algebra.ops[scalin.Vec, scalin.Mat] {

  def defaultTabulate[A](length: Int)(f: Int => A): scalin.Vec[A] =
    scalin.immutable.DenseVec.tabulate[A](length)(f)

  def defaultTabulate[A](rows: Int, cols: Int)(f: (Int, Int) => A): scalin.Mat[A] =
    scalin.immutable.DenseMat.tabulate[A](rows, cols)(f)

}

object dense extends DenseOps
