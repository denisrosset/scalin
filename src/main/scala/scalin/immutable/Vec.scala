package scalin
package immutable

trait Vec[A] extends scalin.Vec[A] {

  def intersectsMutable(mat: AbstractMat[_], rs: Range, cs: Range): Boolean = false

  def intersectsMutable(vec: AbstractVec[_], ks: Range): Boolean = false

}

object Vec extends VecFactory[immutable.Vec, Dummy] {

  def tabulate[A:Dummy](length: Int)( f: Int => A ): immutable.Vec[A] =
    immutable.DenseVec.tabulate[A](length)(f)

  implicit override def fromAbstractVec[A:Dummy](v: AbstractVec[A]): immutable.Vec[A] = v match {
    case iv: immutable.Vec[A] => iv
    case _ => super.fromAbstractVec[A](v)
  }

}
