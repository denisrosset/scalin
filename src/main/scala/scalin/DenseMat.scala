package scalin

abstract class DenseMat[A] extends scalin.Mat[A] {

  def data: Array[AnyRef]

  def apply(r: Int, c: Int) = data(r + c * rows).asInstanceOf[A]

}
