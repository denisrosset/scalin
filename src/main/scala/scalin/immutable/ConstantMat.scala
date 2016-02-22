package scalin
package immutable

case class ConstantMat[A](rows: Int, cols: Int, a: A) extends immutable.Mat[A] {

  def apply(r: Int, c: Int) = a

  def nextNonZeroInCol(r: Int, c: Int): Int = r + 1

  def nextNonZeroInRow(r: Int, c: Int): Int = c + 1

}
