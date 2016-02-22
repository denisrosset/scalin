package scalin
package immutable

trait Mat[A] extends scalin.Mat[A] {

  def intersectsMutable(mat: AbstractMat[_], rs: Range, cs: Range): Boolean = false

  def intersectsMutable(vec: AbstractVec[_], ks: Range): Boolean = false

}

object Mat extends MatFactory[Mat, Dummy] {

  def tabulate[A:Dummy](rows: Int, cols: Int)( f: (Int, Int) => A ): Mat[A] =
    DenseMat.tabulate[A](rows, cols)(f)

  implicit override def fromAbstractMat[A:Dummy](m: AbstractMat[A]): immutable.Mat[A] = m match {
    case im: immutable.Mat[A] => im
    case _ => super.fromAbstractMat[A](m)
  }

}
