package scalin
package immutable

trait Mat[A] extends scalin.Mat[A]

object Mat extends MatFactory[Mat, Dummy] {

  def tabulate[A:Dummy](rows: Int, cols: Int)( f: (Int, Int) => A ): Mat[A] =
    DenseMat.tabulate[A](rows, cols)(f)

  implicit override def fromAbstractMat[A:Dummy](m: AbstractMat[A]): immutable.Mat[A] = m match {
    case im: immutable.Mat[A] => im
    case _ => super.fromAbstractMat[A](m)
  }

}
