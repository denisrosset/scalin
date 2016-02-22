package scalin

abstract class DenseMat[A] extends scalin.Mat[A] {

  type AsImmutable = immutable.DenseMat[A]
  type AsMutable = mutable.DenseMat[A]

  def nextNonZeroInCol(r: Int, c: Int): Int = r + 1

  def nextNonZeroInRow(r: Int, c: Int): Int = c + 1

  def data: Array[AnyRef]

  def apply(r: Int, c: Int) = data(r + c * rows).asInstanceOf[A]

  def mutableCopy: AsMutable = new mutable.DenseMat[A](rows, cols, data.clone)

}
