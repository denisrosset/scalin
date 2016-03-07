package scalin

import spire.syntax.cfor._

abstract class DenseMatFactory[DM[A] <: DenseMat[A]] {

  protected def build[A](rows: Int, cols: Int, data: Array[AnyRef]): DM[A]

  def tabulate[A](rows: Int, cols: Int)(f: (Int, Int) => A): DM[A] = {
    val data = new Array[AnyRef](rows * cols)
    cforRange(0 until rows) { r =>
      cforRange(0 until cols) { c =>
        data(r + c * rows) = f(r, c).asInstanceOf[AnyRef]
      }
    }
    build(rows, cols, data)
  }

}
