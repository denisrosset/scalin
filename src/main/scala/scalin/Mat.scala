package scalin

/** Concrete matrix trait. */
trait Mat[A] { lhs =>

  def apply(r: Int, c: Int): A

  def rows: Int

  def cols: Int

  override def toString: String = Printer.mat(Mat.this)

}

object Mat extends MatFactory[Mat, Dummy] {

  def tabulate[A:Dummy](rows: Int, cols: Int)( f: (Int, Int) => A ): Mat[A] =
    immutable.DenseMat.tabulate[A](rows, cols)(f)

}
