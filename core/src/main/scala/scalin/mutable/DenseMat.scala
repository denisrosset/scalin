package scalin
package mutable

import spire.syntax.cfor._

class DenseMat[A](val rows: Int, val cols: Int, var data: Array[AnyRef]) extends scalin.DenseMat[A] with mutable.Mat[A] {

  def copyIfOverlap(obj: AnyRef) = if (obj eq this) new DenseMat[A](rows, cols, data.clone) else this

  def set(r: Int, c: Int, a: A): Unit = {
    data(r + c * rows) = a.asInstanceOf[AnyRef]
  }

  def exported = (data eq null)

  def result(): immutable.DenseMat[A] = {
    val res = new immutable.DenseMat[A](rows, cols, data)
    data = null
    res
  }

}

object DenseMat extends scalin.DenseMatFactory[DenseMat] {

  def build[A](rows: Int, cols: Int, data: Array[AnyRef]): DenseMat[A] =
    new DenseMat[A](rows, cols, data)

}
