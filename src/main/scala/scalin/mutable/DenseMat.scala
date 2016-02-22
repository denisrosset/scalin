package scalin
package mutable

import spire.syntax.cfor._

class DenseMat[A](val rows: Int, val cols: Int, var data: Array[AnyRef]) extends scalin.DenseMat[A] with mutable.Mat[A] with Builder {

  def update(r: Int, c: Int, a: A): Unit = {
    data(r + c * rows) = a.asInstanceOf[AnyRef]
  }

  def toImmutable: AsImmutable = new immutable.DenseMat[A](rows, cols, data.clone)

  def exported = (data eq null)

  def result(): AsImmutable = {
    val res = new immutable.DenseMat[A](rows, cols, data)
    data = null
    res
  }

  def :=(rhs: AbstractMat[A]): Unit = rhs.touch(DenseMat.this) match {
    case Touch.Clean() | Touch.AsIs() =>
      cforRange(0 until rows) { r =>
        cforRange(0 until cols) { c =>
          update(r, c, rhs(r, c))
        }
      }
    case _ =>
      val res = mutableCopy // TODO: be clever
      res := rhs
      cforRange(0 until rows) { r =>
        cforRange(0 until cols) { c =>
          update(r, c, res(r, c))
        }
      }
  }
}

object DenseMat extends scalin.DenseMatFactory[DenseMat] {

  def build[A](rows: Int, cols: Int, data: Array[AnyRef]): DenseMat[A] =
    new DenseMat[A](rows, cols, data)

}
