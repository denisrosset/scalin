object ops extends scalin.algebra.ops[scalin.Vec, scalin.Mat] {

  def defaultTabulate[A](length: Int)(f: Int => A): scalin.Vec[A] =
    scalin.Vec.tabulate(length)(f)

  def defaultTabulate[A](rows: Int, cols: Int)(f: (Int, Int) => A): scalin.Mat[A] =
    scalin.Mat.tabulate(rows, cols)(f)

}
